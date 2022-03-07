{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Notification where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Exception
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty
import Data.UUID (UUID)
import GHC.Conc (TVar, atomically, readTVar, writeTVar)
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
  | WSNotificationConnectionBroken

wsApp :: Unagi.InChan WSNotification -> TVar Int -> WS.ClientApp ()
wsApp chan missedPings conn = do
  WS.withPingThread conn 30 (onPing chan missedPings) $
    loop `catch` \case
      WS.CloseRequest _ _ -> Unagi.writeChan chan WSNotificationClose
      WS.ConnectionClosed -> Unagi.writeChan chan WSNotificationClose
      WS.ParseException str -> error $ "Parse Exception: " <> show str
      exc -> error $ "Other Exception: " <> show exc
  where
    loop = do
      bs <- WS.receiveData conn
      Unagi.writeChan chan
        . either (WSNotificationInvalid bs) WSNotification
        $ Aeson.eitherDecode bs
      loop

onPong :: TVar Int -> IO ()
onPong missedPings = atomically $ writeTVar missedPings 0

onPing :: Unagi.InChan WSNotification -> TVar Int -> IO ()
onPing chan missedPings = do
  missed <- atomically $ do
    missed <- readTVar missedPings
    writeTVar missedPings (missed + 1)
    pure (missed + 1)
  when (missed > 3) $ Unagi.writeChan chan WSNotificationConnectionBroken
