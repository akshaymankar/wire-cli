{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Wire.CLI.Backend.Credential where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64 as Base64
import Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Wire.CLI.Util.ByteStringJSON as BSJSON
import Wire.CLI.Util.JSONStrategy

data LoginResponse
  = LoginSuccess Credential
  | LoginFailure Text

newtype WireCookie = WireCookie {unWireCookie :: HTTP.Cookie}
  deriving (Show, Eq, Generic)

data ServerCredential = ServerCredential
  { server :: URI,
    credential :: Credential
  }
  deriving (Show, Eq, Generic)

data Credential = Credential
  { credentialCookies :: [WireCookie],
    credentialAccessToken :: AccessToken
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "credential" Credential

data AccessToken = AccessToken
  { expiresIn :: Time.DiffTime,
    accessToken :: Text,
    user :: Text,
    tokenType :: TokenType
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "" AccessToken

data TokenType = TokenTypeBearer
  deriving (Show, Eq, Generic)

instance FromJSON TokenType where
  parseJSON = Aeson.withText "TokenType" $ \case
    "Bearer" -> pure TokenTypeBearer
    t -> fail $ "Invalid TokenType: " <> show t

instance ToJSON TokenType where
  toJSON TokenTypeBearer = Aeson.String "Bearer"

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

instance FromJSON ServerCredential where
  parseJSON = Aeson.withObject "Server Credential" $ \o ->
    ServerCredential
      <$> (uriFromJSON =<< o .: "server")
      <*> o .: "credential"

instance ToJSON ServerCredential where
  toJSON sc =
    object
      [ "server" .= URI.uriToString id (server sc) "",
        "credential" .= credential sc
      ]

uriFromJSON :: String -> Aeson.Parser URI
uriFromJSON s = case URI.parseURI s of
  Nothing -> fail $ "Expected URI, got: " <> s
  Just u -> pure u
