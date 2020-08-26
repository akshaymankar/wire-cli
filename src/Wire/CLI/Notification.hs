{-# LANGUAGE RecordWildCards #-}

module Wire.CLI.Notification where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as UUID
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Backend.Event as Event
import Wire.CLI.Backend.Notification
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

sync :: Members '[Backend, Store, Error WireCLIError] r => Sem r ()
sync = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  client <-
    Store.getClientId
      >>= Error.note (WireCLIError.ErrorInvalidState WireCLIError.NoClientFound)

  lastNotification <- fromMaybe (NotificationId UUID.nil) <$> Store.getLastNotificationId
  void $ getAll lastNotification $ Backend.getNotifications serverCreds 1000 client

getAll :: Members '[Backend, Store] r => NotificationId -> (NotificationId -> Sem r (NotificationGap, Notifications)) -> Sem r [Notification]
getAll initial f = loop initial
  where
    loop x = do
      (_, Notifications hasMore notifs) <- f x
      mapM_ process notifs
      let nextLastNotifId = notificationId $ last notifs
      Store.saveLastNotificationId nextLastNotifId
      if hasMore
        then (notifs <>) <$> loop nextLastNotifId
        else pure notifs

process :: Members '[Store] r => Notification -> Sem r ()
process = mapM_ processEvent . notificationPayload

processEvent :: Members '[Store] r => Event.ExtensibleEvent -> Sem r ()
processEvent = \case
  Event.UnknownEvent _ _ -> pure ()
  Event.KnownEvent e -> do
    case e of
      Event.EventUser u -> processUserEvent u
      Event.EventUserProperty _ -> pure ()
      Event.EventConv _ -> pure ()
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
