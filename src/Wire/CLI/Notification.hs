{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wire.CLI.Notification where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Monad (void)
import qualified Data.ByteString.Base64 as Base64
import Data.Functor (($>))
import Data.Id (ConvId, UserId)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Qualified
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import qualified Wire.API.Event.Conversation as Conv
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend.Effect as Backend
import qualified Wire.CLI.Backend.Event as Event
import Wire.CLI.Backend.Notification
import Wire.CLI.Chan (ReadChan, WriteChan, readChan, writeChan)
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Message (decryptMessage, mkSessionId)
import Wire.CLI.Notification.Types
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

sync :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Sem r [ProcessedNotification]
sync = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  client <-
    Store.getClientId
      >>= Error.note (WireCLIError.ErrorInvalidState WireCLIError.NoClientFound)

  lastNotification <- fromMaybe (NotificationId UUID.nil) <$> Store.getLastNotificationId
  getAll lastNotification $ Backend.getNotifications serverCreds 1000 client

getAll ::
  Members '[Backend, Store, CryptoBox, Error WireCLIError] r =>
  NotificationId ->
  (NotificationId -> Sem r (NotificationGap, Notifications)) ->
  Sem r [ProcessedNotification]
getAll initial f = loop initial
  where
    loop x = do
      (_, Notifications hasMore notifs) <- f x
      ns <- concatMap NonEmpty.toList <$> mapM process notifs
      if null notifs
        then pure ns
        else do
          let nextLastNotifId = notificationId $ last notifs
          if hasMore
            then (ns <>) <$> loop nextLastNotifId
            else pure ns

{-# HLINT ignore "Use writeList2Chan" #-}
watch :: forall r. (Members '[Store, CryptoBox, Backend, Error WireCLIError, ReadChan, WriteChan] r) => Unagi.InChan ProcessedNotification -> Sem r ()
watch processedChan = do
  -- Sync before subscribing to the websocket to reduce the chance of missed messages
  syncedNotifs <- sync
  mapM_ (writeChan processedChan) syncedNotifs

  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  client <-
    Store.getClientId
      >>= Error.note (WireCLIError.ErrorInvalidState WireCLIError.NoClientFound)
  reader <- Backend.watchNotifications serverCreds (Just client)
  readUntilClose reader
  where
    readUntilClose :: Unagi.OutChan WSNotification -> Sem r ()
    readUntilClose reader = do
      res <- readChan reader
      case res of
        Right (WSNotification n) -> do
          processed <- process n
          mapM_ (writeChan processedChan) processed
          readUntilClose reader
        Right (WSNotificationInvalid _err _bs) ->
          -- TODO: log the error
          readUntilClose reader
        Right WSNotificationClose ->
          pure ()
        Left _ ->
          pure ()

process :: Members '[Store, CryptoBox] r => Notification -> Sem r (NonEmpty ProcessedNotification)
process n =
  mapM processEvent (notificationPayload n) <* Store.saveLastNotificationId (notificationId n)

processEvent :: Members '[Store, CryptoBox] r => Event.ExtensibleEvent -> Sem r ProcessedNotification
processEvent e = case e of
  Event.UnknownEvent _ _ -> pure () $> PlainNotification e
  Event.KnownEvent ke -> do
    case ke of
      Event.EventUser u -> processUserEvent u $> PlainNotification e
      Event.EventUserProperty _ -> pure () $> PlainNotification e
      Event.EventConv c -> processConvEvent c
      Event.EventTeam _ -> pure () $> PlainNotification e

processUserEvent :: Members '[Store] r => Event.UserEvent -> Sem r ()
processUserEvent = \case
  Event.EventUserConnection Event.ConnectionEvent {..} ->
    Store.addConnection connectionEventConnection
  Event.EventUserUpdate _ -> pure ()
  Event.EventUserIdentityRemove _ -> pure ()
  Event.EventUserPushRemove _ -> pure ()
  Event.EventUserDelete _ -> pure ()
  Event.EventUserClientAdd _ -> pure ()
  Event.EventUserClientRemove _ -> pure ()

processConvEvent :: Members '[Store, CryptoBox] r => Conv.Event -> Sem r ProcessedNotification
processConvEvent e@Conv.Event {..} =
  case evtData of
    Conv.EdConversation _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdConvDelete -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdConvRename _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdMembersJoin _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdMembersLeave _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdMemberUpdate _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdConnect _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdTyping _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdOtrMessage msg ->
      addOtrMessage evtConv evtFrom evtTime msg
    Conv.EdConvAccessUpdate _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdConvCodeUpdate _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdConvCodeDelete -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdConvReceiptModeUpdate _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))
    Conv.EdConvMessageTimerUpdate _ -> pure () $> PlainNotification (Event.KnownEvent (Event.EventConv e))

-- TODO: Only decode messages meant for the client
addOtrMessage ::
  Members '[Store, CryptoBox] r =>
  Qualified ConvId ->
  Qualified UserId ->
  UTCTime ->
  Conv.OtrMessage ->
  Sem r ProcessedNotification
addOtrMessage conv user time otr@Conv.OtrMessage {..} =
  runErrorWith (saveInvalidMesssge conv user time otr) $ do
    cipherBS <- errorFromEitherWith Text.unpack $ Base64.decodeBase64 (Text.encodeUtf8 otrCiphertext)
    (ses, messageBS) <-
      decryptMessage (mkSessionId user otrSender) cipherBS
        >>= errorFromEitherWith (\e -> "failed to decrypt: " <> show e)

    let msg = Store.StoredMessage user otrSender time $ Store.decodeMessage messageBS
    Store.addMessage conv msg
    -- TODO: Log here if saving session fails
    void $ CryptoBox.save ses
    pure $ DecryptedMessage conv msg

runErrorWith :: (e -> Sem r a) -> Sem (Error e ': r) a -> Sem r a
runErrorWith f action = either f pure =<< Error.runError action

errorFromEitherWith :: Member (Error e2) r => (e1 -> e2) -> Either e1 a -> Sem r a
errorFromEitherWith f = Error.mapError f . Error.fromEither

saveInvalidMesssge ::
  Member Store r =>
  Qualified ConvId ->
  Qualified UserId ->
  UTCTime ->
  Conv.OtrMessage ->
  String ->
  Sem r ProcessedNotification
saveInvalidMesssge conv user time Conv.OtrMessage {..} invalid = do
  let msg = Store.StoredMessage user otrSender time . Store.InvalidMessage $ invalid
  Store.addMessage conv msg
  pure $ DecryptedMessage conv msg
