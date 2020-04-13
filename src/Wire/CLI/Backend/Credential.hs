module Wire.CLI.Backend.Credential where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as HTTP
import qualified Wire.CLI.Util.ByteStringJSON as BSJSON

data LoginResponse
  = LoginSuccess Credential
  | LoginFailure Text

newtype WireCookie = WireCookie HTTP.Cookie
  deriving (Show, Eq)

data Credential
  = Credential
      { cookies :: [WireCookie],
        accessToken :: AccessToken
      }
  deriving (Show, Eq)

data AccessToken
  = AccessToken
      { expiresIn :: Time.DiffTime,
        token :: Text,
        user :: Text,
        tokenType :: TokenType
      }
  deriving (Show, Eq)

data TokenType = TokenTypeBearer
  deriving (Show, Eq)

instance FromJSON TokenType where
  parseJSON = Aeson.withText "TokenType" $ \case
    "Bearer" -> pure TokenTypeBearer
    t -> fail $ "Invalid TokenType: " <> show t

instance ToJSON TokenType where
  toJSON TokenTypeBearer = Aeson.String "Bearer"

instance FromJSON AccessToken where
  parseJSON = Aeson.withObject "AccessToken" $ \o ->
    AccessToken
      <$> o .: "expires_in"
      <*> o .: "access_token"
      <*> o .: "user"
      <*> o .: "token_type"

instance ToJSON AccessToken where
  toJSON t =
    object
      [ "expires_in" .= expiresIn t,
        "access_token" .= token t,
        "user" .= user t,
        "token_type" .= tokenType t
      ]

instance FromJSON Credential where
  parseJSON = Aeson.withObject "Credential" $ \o ->
    Credential
      <$> o .: "cookies"
      <*> o .: "access_token"

instance ToJSON Credential where
  toJSON cred =
    object
      [ "cookies" .= cookies cred,
        "access_token" .= accessToken cred
      ]

instance FromJSON WireCookie where
  parseJSON =
    Aeson.withObject "Wire Cookie" $ \o -> do
      cookie <-
        HTTP.Cookie
          <$> (BSJSON.parseBase64 =<< o .: "name")
            <*> (BSJSON.parseBase64 =<< o .: "value")
            <*> o .: "expiry_time"
            <*> (BSJSON.parseBase64 =<< o .: "domain")
            <*> (BSJSON.parseBase64 =<< o .: "path")
            <*> o .: "creation_time"
            <*> o .: "last_access_time"
            <*> o .: "persistent"
            <*> o .: "host_only"
            <*> o .: "secure_only"
            <*> o .: "http_only"
      pure $ WireCookie cookie

instance ToJSON WireCookie where
  toJSON (WireCookie cookie) =
    object
      [ "name" .= Base64.encodeBase64 (HTTP.cookie_name cookie),
        "value" .= Base64.encodeBase64 (HTTP.cookie_value cookie),
        "expiry_time" .= HTTP.cookie_expiry_time cookie,
        "domain" .= Base64.encodeBase64 (HTTP.cookie_domain cookie),
        "path" .= Base64.encodeBase64 (HTTP.cookie_path cookie),
        "creation_time" .= HTTP.cookie_creation_time cookie,
        "last_access_time" .= HTTP.cookie_last_access_time cookie,
        "persistent" .= HTTP.cookie_persistent cookie,
        "host_only" .= HTTP.cookie_host_only cookie,
        "secure_only" .= HTTP.cookie_secure_only cookie,
        "http_only" .= HTTP.cookie_http_only cookie
      ]
