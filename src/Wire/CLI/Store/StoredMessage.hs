{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Wire.CLI.Store.StoredMessage where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Id (ClientId, UserId)
import qualified Data.ProtoLens as Proto
import Data.Qualified
import Data.Time (UTCTime)
import Proto.Messages (GenericMessage)
import Wire.CLI.Util.ByteStringJSON (Base64ByteString (..))
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
    ValidMessage msg -> Aeson.object ["ValidMessage" .= (Base64ByteString . Proto.encodeMessage $ msg)]
    InvalidMessage reason -> Aeson.object ["InvalidMessage" .= reason]

instance FromJSON StoredMessageData where
  parseJSON = Aeson.withObject "StoredMessageData" $ \o -> do
    maybeInvalidReason <- o .:? "InvalidMessage"
    case maybeInvalidReason of
      Just reason -> pure $ InvalidMessage reason
      Nothing ->
        decodeMessage . unpackBase64ByteString <$> o .: "ValidMessage"

decodeMessage :: ByteString -> StoredMessageData
decodeMessage bs = case Proto.decodeMessage bs of
  Left err -> InvalidMessage $ "failed to decode protobuf with: " <> err
  Right msg -> ValidMessage msg
