{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Client where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Wire.CLI.Backend.Prekey
import Wire.CLI.Util.JSONStrategy

data ClientClass
  = Desktop
  | Mobile
  deriving (Show, Eq, Generic)

instance ToJSON ClientClass where
  toJSON = \case
    Desktop -> Aeson.String "desktop"
    Mobile -> Aeson.String "mobile"

instance FromJSON ClientClass where
  parseJSON =
    Aeson.withText "Client Type" $ \case
      "desktop" -> pure Desktop
      "mobile" -> pure Mobile
      x -> fail $ "Invalid ClientType: " ++ show x

data ClientType
  = Permanent
  | Temporary
  deriving (Show, Eq, Generic)

instance ToJSON ClientType where
  toJSON = \case
    Permanent -> Aeson.String "permanent"
    Temporary -> Aeson.String "temporary"

instance FromJSON ClientType where
  parseJSON =
    Aeson.withText "Client Class" $ \case
      "permanent" -> pure Permanent
      "temporary" -> pure Temporary
      x -> fail $ "Invalid ClientClass: " ++ show x

-- | Specification also requires @sigkeys@ to be implemented for mobile devices, but it is not
-- implemented here
data NewClient = NewClient
  { -- | cookie label
    newClientCookie :: Text,
    newClientLastkey :: Prekey,
    newClientPassword :: Text,
    newClientModel :: Text,
    newClientType :: ClientType,
    newClientPrekeys :: [Prekey],
    newClientClass :: ClientClass,
    -- | User friendly name
    newClientLabel :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "newClient" NewClient

newtype ClientId = ClientId Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Client = Client
  { clientId :: ClientId,
    -- | cookie label
    clientCookie :: Text,
    clientModel :: Text,
    clientType :: ClientType,
    clientClass :: ClientClass,
    -- | User friendly name
    clientLabel :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "client" Client
