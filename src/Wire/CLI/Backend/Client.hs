{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Client where

import Data.Aeson (FromJSON (..), ToJSON (..))
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
    cookie :: Text,
    lastkey :: Prekey,
    password :: Text,
    model :: Text,
    typ :: ClientType,
    prekeys :: [Prekey],
    clas :: ClientClass,
    -- | User friendly name
    label :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy NewClient

