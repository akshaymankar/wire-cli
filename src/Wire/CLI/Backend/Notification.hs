{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Notification where

import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UnagiNB
import Control.Exception (catch)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty
import Data.UUID (UUID)
import qualified Network.WebSockets as WS
import Wire.CLI.Backend.Event (ExtensibleEvent)
import Wire.CLI.Util.JSONStrategy

newtype NotificationId = NotificationId UUID
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Notifications = Notifications
  { notificationsHasMore :: Bool,
    notificationsNotifications :: [Notification]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "notifications" Notifications

data Notification = Notification
  { notificationId :: NotificationId,
    notificationPayload :: NonEmpty ExtensibleEvent
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "notification" Notification

-- | Existence of gap implies, missed notifications
data NotificationGap
  = NotificationGapExists
  | NotificationGapDoesNotExist

data WSNotification
  = WSNotification Notification
  | WSNotificationInvalid LBS.ByteString String
  | WSNotificationClose

wsApp :: UnagiNB.InChan WSNotification -> WS.ClientApp ()
wsApp chan conn =
  WS.withPingThread conn 30 (pure ()) $
    loop `catch` \case
      WS.CloseRequest _ _ -> UnagiNB.writeChan chan WSNotificationClose
      WS.ConnectionClosed -> UnagiNB.writeChan chan WSNotificationClose
      WS.ParseException str -> error $ "Parse Exception: " <> show str
      exc -> error $ "Other Exception: " <> show exc
  where
    loop = do
      bs <- WS.receiveData conn
      UnagiNB.writeChan chan
        . either (WSNotificationInvalid bs) WSNotification
        $ Aeson.eitherDecode bs
      loop
