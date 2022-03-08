{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Notification where

import Control.Concurrent
import Control.Concurrent.Async (race)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Exception
import Control.Monad (forever, when)
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

-- Some cases of this code cannot be trivially tested, few test cases that can
-- be carried out manually when this code changes:
--
-- 1. Ensure watch stops on connection failure: Start watching
-- notifications, disconnect from internet, see that the watch stops
-- automatically after some time (~2 minutes).
--
-- 2. Ensure connections are closed when pongs are not received: Change code in
-- 'executeAndPrint' so that the function doesn't exit after notification thread
-- is done, maybe add a 'putStrLn' followed by a very long 'threadDelay'. Start
-- watching notifications, disconnect from internet, see that the watch stops
-- after some time, run 'ss dst = <ip-address-of-cannon>', ensure that the
-- connection is not in 'established' state.
wsApp :: Unagi.InChan WSNotification -> TVar Int -> WS.ClientApp ()
wsApp chan pingsSinceLastPong conn = do
  brokenConn <- newEmptyMVar
  WS.withPingThread conn 30 (onPing pingsSinceLastPong brokenConn) $ do
    loopRet <- race (takeMVar brokenConn) loopWithExceptions
    case loopRet of
      Left () -> Unagi.writeChan chan WSNotificationConnectionBroken
      Right () -> pure ()
  where
    loopWithExceptions =
      loop `catch` \case
        WS.CloseRequest _ _ -> Unagi.writeChan chan WSNotificationClose
        WS.ConnectionClosed -> Unagi.writeChan chan WSNotificationClose
        WS.ParseException str -> error $ "Parse Exception: " <> show str
        exc -> error $ "Other Exception: " <> show exc
    loop = forever $ do
      bs <- WS.receiveData conn
      Unagi.writeChan chan
        . either (WSNotificationInvalid bs) WSNotification
        $ Aeson.eitherDecode bs

onPong :: TVar Int -> IO ()
onPong pingsSinceLastPong =
  atomically $ writeTVar pingsSinceLastPong 0

onPing :: TVar Int -> MVar () -> IO ()
onPing pingsSinceLastPong connBroken = do
  missingPongs <- atomically $ do
    missingPongs <- readTVar pingsSinceLastPong
    writeTVar pingsSinceLastPong (missingPongs + 1)
    pure (missingPongs + 1)
  when (missingPongs > 3) $ putMVar connBroken ()
