{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Wire.CLI.Backend.Connection where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.User
import Wire.CLI.Util.JSONStrategy

data ConnectionList = ConnectionList
  { connectionListConnections :: [Connection],
    connectionListHasMore :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSONStrategy "connectionList" ConnectionList

data Connection = Connection
  { connectionConvId :: ConvId,
    connectionFrom :: UserId,
    connectionTo :: UserId,
    connectionMessage :: Maybe Text,
    connectionStatus :: Relation,
    connectionLastUpdated :: UTCTime
    -- Android client also has: fromUserName :: Maybe Name
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSONStrategy "connection" Connection

data Relation
  = Accepted
  | Blocked
  | Pending
  | Ignored
  | Sent
  | Cancelled
  deriving (Show, Eq, Generic)

instance FromJSON Relation where
  parseJSON = Aeson.withText "Relation" $ \case
    "accepted" -> pure Accepted
    "blocked" -> pure Blocked
    "pending" -> pure Pending
    "ignored" -> pure Ignored
    "sent" -> pure Sent
    "cancelled" -> pure Cancelled
    t -> fail $ "invalid relation: " <> show t

instance ToJSON Relation where
  toJSON = Aeson.String . \case
    Accepted -> "accepted"
    Blocked -> "blocked"
    Pending -> "pending"
    Ignored -> "ignored"
    Sent -> "sent"
    Cancelled -> "cancelled"
