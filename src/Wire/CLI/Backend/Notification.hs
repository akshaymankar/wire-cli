{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Notification where

import qualified Data.Aeson as Aeson
import Data.List.NonEmpty
import Data.UUID (UUID)
import Wire.CLI.Util.JSONStrategy

newtype NotificationId = NotificationId UUID
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Notifications = Notifications
  { notificationsHasMore :: Bool,
    notificationsNotifications :: [Notification]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "notifications" Notifications

data Notification = Notification
  { notificationId :: NotificationId,
    notificationPayload :: NonEmpty Aeson.Object
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "notification" Notification

data NotificationGap
  = NotificationGapExists
  | NotificationGapDoesNotExist
