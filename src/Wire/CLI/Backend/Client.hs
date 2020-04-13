module Wire.CLI.Backend.Client where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Wire.CLI.Backend.Prekey

data ClientClass
  = Desktop
  | Mobile
  deriving (Show, Eq)

data ClientType
  = Permanent
  | Temporary
  deriving (Show, Eq)

-- | Specification also requires @sigkeys@ to be implemented for mobile devices, but it is not
-- implemented here
data NewClient
  = NewClient
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
  deriving (Show, Eq)

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

instance ToJSON NewClient where
  toJSON c =
    Aeson.object
      [ "cookie" .= cookie c,
        "lastkey" .= lastkey c,
        "password" .= password c,
        "model" .= model c,
        "type" .= typ c,
        "prekeys" .= prekeys c,
        "class" .= clas c,
        "label" .= label c
      ]

instance FromJSON NewClient where
  parseJSON =
    Aeson.withObject "NewClient" $ \o ->
      NewClient
        <$> o .: "cookie"
        <*> o .: "lastkey"
        <*> o .: "password"
        <*> o .: "model"
        <*> o .: "type"
        <*> o .: "prekeys"
        <*> o .: "class"
        <*> o .: "label"
