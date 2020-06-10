{-# LANGUAGE DeriveGeneric #-}

module Wire.CLI.Backend.Prekey where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Word
import GHC.Generics
import Wire.CLI.Util.ByteStringJSON (Base64ByteString)

data Prekey = Prekey
  { _id :: Word16,
    key :: Base64ByteString
  }
  deriving (Show, Eq, Generic)

instance FromJSON Prekey where
  parseJSON = Aeson.withObject "Prekey" $ \o ->
    Prekey
      <$> o .: "id"
      <*> o .: "key"

instance ToJSON Prekey where
  toJSON k =
    Aeson.object
      [ "id" .= _id k,
        "key" .= key k
      ]
