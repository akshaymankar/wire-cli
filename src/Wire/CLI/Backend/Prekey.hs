module Wire.CLI.Backend.Prekey where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Word
import qualified Wire.CLI.Util.ByteStringJSON as BSJSON

data Prekey
  = Prekey
      { _id :: Word16,
        key :: ByteString
      }
  deriving (Show, Eq)

instance FromJSON Prekey where
  parseJSON = Aeson.withObject "Prekey" $ \o ->
    Prekey
      <$> o .: "id"
      <*> (BSJSON.parseBase64 =<< o .: "key")

instance ToJSON Prekey where
  toJSON k =
    Aeson.object
      [ "id" .= _id k,
        "key" .= Base64.encodeBase64 (key k)
      ]
