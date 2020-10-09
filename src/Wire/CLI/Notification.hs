{-# LANGUAGE RecordWildCards #-}

module Wire.CLI.Notification where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.CLI.Backend (Backend, ConvId, UserId)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Backend.Event as Event
import Wire.CLI.Backend.Notification
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Message (decryptMessage, mkSessionId)
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.CLI.Util.ByteStringJSON (Base64ByteString (..), unpackBase64ByteString)

sync :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Sem r ()
sync = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  client <-
    Store.getClientId
      >>= Error.note (WireCLIError.ErrorInvalidState WireCLIError.NoClientFound)

  lastNotification <- fromMaybe (NotificationId UUID.nil) <$> Store.getLastNotificationId
  void $ getAll lastNotification $ Backend.getNotifications serverCreds 1000 client

getAll :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => NotificationId -> (NotificationId -> Sem r (NotificationGap, Notifications)) -> Sem r [Notification]
getAll initial f = loop initial
  where
    loop x = do
      (_, Notifications hasMore notifs) <- f x
      mapM_ process notifs
      if null notifs
        then pure notifs
        else do
          let nextLastNotifId = notificationId $ last notifs
          Store.saveLastNotificationId nextLastNotifId
          if hasMore
            then (notifs <>) <$> loop nextLastNotifId
            else pure notifs

process :: Members '[Store, CryptoBox, Error WireCLIError] r => Notification -> Sem r ()
process = mapM_ processEvent . notificationPayload

processEvent :: Members '[Store, CryptoBox, Error WireCLIError] r => Event.ExtensibleEvent -> Sem r ()
processEvent = \case
  Event.UnknownEvent _ _ -> pure ()
  Event.KnownEvent e -> do
    case e of
      Event.EventUser u -> processUserEvent u
      Event.EventUserProperty _ -> pure ()
      Event.EventConv c -> processConvEvent c
      Event.EventTeam _ -> pure ()

processUserEvent :: Members '[Store] r => Event.UserEvent -> Sem r ()
processUserEvent = \case
  Event.EventUserConnection (Event.ConnectionEvent {..}) ->
    Store.addConnection connectionEventConnection
  Event.EventUserUpdate _ -> pure ()
  Event.EventUserIdentityRemove _ -> pure ()
  Event.EventUserPushRemove _ -> pure ()
  Event.EventUserDelete _ -> pure ()
  Event.EventUserClientAdd _ -> pure ()
  Event.EventUserClientRemove _ -> pure ()

processConvEvent :: Members '[Store, CryptoBox, Error WireCLIError] r => Event.ConvEvent -> Sem r ()
processConvEvent Event.ConvEvent {..} =
  case convEventData of
    Event.EventConvCreate _ -> pure ()
    Event.EventConvDelete -> pure ()
    Event.EventConvRename _ -> pure ()
    Event.EventConvMemberJoin _ -> pure ()
    Event.EventConvMemberLeave _ -> pure ()
    Event.EventConvMemberUpdate _ -> pure ()
    Event.EventConvConnectRequest _ -> pure ()
    Event.EventConvTyping _ -> pure ()
    Event.EventConvOtrMessageAdd msg ->
      addOtrMessage convEventConversation convEventFrom convEventTime msg
    Event.EventConvAccessUpdate _ -> pure ()
    Event.EventConvCodeUpdate _ -> pure ()
    Event.EventConvCodeDelete -> pure ()
    Event.EventConvRecieptModeUpdate _ -> pure ()
    Event.EventConvMessageTimerUpdate _ -> pure ()
    Event.EventConvGenericMessage -> pure ()
    Event.EventConvOtrError _ -> pure ()

-- TODO: Only decode messages meant for the client
addOtrMessage :: Members '[Store, CryptoBox, Error WireCLIError] r => ConvId -> UserId -> UTCTime -> Event.OtrMessage -> Sem r ()
addOtrMessage conv user time (Event.OtrMessage {..}) = do
  (ses, messageBS) <- decryptMessage (mkSessionId user otrSender) (unpackBase64ByteString otrText)
  Store.addMessage conv (Store.StoredMessage user otrSender time $ Store.decodeMessage messageBS)
  CryptoBox.save ses
