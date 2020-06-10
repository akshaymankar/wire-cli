module Wire.CLI.Util.ByteStringJSON where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

parseBase64 :: Text -> Aeson.Parser ByteString
parseBase64 b64Text =
  case Base64.decodeBase64 $ Text.encodeUtf8 b64Text of
    Left err -> fail $ "Failed to decode base64 with error: " <> Text.unpack err
    Right bs -> pure bs

newtype Base64ByteString = Base64ByteString ByteString
  deriving (Show, Eq)

instance FromJSON Base64ByteString where
  parseJSON = Aeson.withText "Base64ByteString" (fmap Base64ByteString . parseBase64)

instance ToJSON Base64ByteString where
  toJSON (Base64ByteString b) = Aeson.String $ Base64.encodeBase64 b
