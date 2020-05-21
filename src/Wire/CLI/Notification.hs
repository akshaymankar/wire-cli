module Wire.CLI.Notification where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.UUID as UUID
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
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
      let nextLastNotifId = notificationId $ last notifs
      Store.saveLastNotificationId nextLastNotifId
      if hasMore
        then (notifs <>) <$> loop nextLastNotifId
        else pure notifs
