{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Wire.CLI.Store.StoredMessage where

import Data.Text (Text)
import Data.Time (UTCTime)
import Wire.CLI.Backend.Client (ClientId)
import Wire.CLI.Backend.User (UserId)
import Wire.CLI.Util.JSONStrategy

data StoredMessage = StoredMessage
  { smSenderUser :: UserId,
    smSenderClient :: ClientId,
    smSentAt :: UTCTime,
    smMessage :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSONStrategy "sm" StoredMessage
