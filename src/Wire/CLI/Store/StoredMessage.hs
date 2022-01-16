{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Wire.CLI.Store.StoredMessage where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import Data.Id (ClientId, UserId)
import qualified Data.ProtoLens as Proto
import Data.Qualified
import Data.Schema hiding ((.=))
import Data.Time (UTCTime)
import Proto.Messages hiding (Text)
import Wire.CLI.Store.StoredMessage.JSON
import Wire.CLI.Util.JSONStrategy (CustomJSON (..), Generic, JSONStrategy)

data StoredMessage = StoredMessage
  { smSenderUser :: Qualified UserId,
    smSenderClient :: ClientId,
    smSentAt :: UTCTime,
    smMessage :: StoredMessageData
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via JSONStrategy "sm" StoredMessage

data StoredMessageData
  = InvalidMessage String
  | ValidMessage GenericMessage
  deriving (Show, Eq, Generic)

instance ToJSON StoredMessageData where
  toJSON = \case
    ValidMessage msg -> Aeson.object ["ValidMessage" Aeson..= schemaOut genericMessageSchema msg]
    InvalidMessage reason -> Aeson.object ["InvalidMessage" Aeson..= reason]

instance FromJSON StoredMessageData where
  parseJSON = Aeson.withObject "StoredMessageData" $ \o -> do
    maybeInvalidReason <- o .:? "InvalidMessage"
    case maybeInvalidReason of
      Just reason -> pure $ InvalidMessage reason
      Nothing -> ValidMessage <$> Aeson.explicitParseField (schemaIn genericMessageSchema) o "ValidMessage"

decodeMessage :: ByteString -> StoredMessageData
decodeMessage bs = case Proto.decodeMessage bs of
  Left err -> InvalidMessage $ "failed to decode protobuf with: " <> err
  Right msg -> ValidMessage msg
