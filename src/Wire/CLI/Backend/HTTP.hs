module Wire.CLI.Backend.HTTP where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Network.HTTP.Client (cookieJar, method, path, queryString, requestBody, requestHeaders)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI)
import Numeric.Natural
import Polysemy
import Wire.CLI.Backend.Client (Client, ClientId (..), NewClient)
import Wire.CLI.Backend.Conv (ConvId (..), Convs)
import Wire.CLI.Backend.Credential (AccessToken (..), Credential (..), LoginResponse (..), ServerCredential (ServerCredential), WireCookie (..))
import qualified Wire.CLI.Backend.Credential as Credential
import Wire.CLI.Backend.Effect
import Wire.CLI.Backend.Notification (NotificationGap (..), NotificationId (..), Notifications)
import qualified Wire.CLI.Options as Opts

-- TODO: Get rid of all the 'error' calls
run :: Member (Embed IO) r => Text -> HTTP.Manager -> Sem (Backend ': r) a -> Sem r a
run label mgr = interpret $
  embed . \case
    Login opts -> runLogin label mgr opts
    RegisterClient serverCred client -> runRegisterClient mgr serverCred client
    ListConvs serverCred size start -> runListConvs mgr serverCred size start
    GetNotifications serverCred size since client -> runGetNotifications mgr serverCred size since client
    RegisterWireless opts -> runRegisterWireless mgr opts
    RefreshToken server cookies -> runRefreshToken mgr server cookies

runLogin :: Text -> HTTP.Manager -> Opts.LoginOptions -> IO LoginResponse
runLogin label mgr (Opts.LoginOptions server handle password) = do
  let body =
        Aeson.object
          [ "handle" .= handle,
            "password" .= password,
            "label" .= label
          ]
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode body,
            path = "/login",
            requestHeaders = [(HTTP.hContentType, "application/json")]
          }
  HTTP.withResponse request mgr handleLogin
  where
    handleLogin response = do
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      let status = HTTP.responseStatus response
      if status /= HTTP.status200
        then pure $ LoginFailure $ "Login failed with status " <> Text.pack (show status) <> " and Body " <> Text.pack (show bodyText)
        else case Aeson.decodeStrict bodyText of
          Nothing -> pure $ LoginFailure "Failed to decode access token"
          Just t -> do
            let c = map WireCookie $ HTTP.destroyCookieJar $ HTTP.responseCookieJar response
            pure $ LoginSuccess $ Credential c t

runRegisterClient :: HTTP.Manager -> ServerCredential -> NewClient -> IO Client
runRegisterClient mgr (ServerCredential server cred) newClient = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode newClient,
            path = "/clients",
            requestHeaders =
              [ (HTTP.hContentType, "application/json"),
                (HTTP.hAuthorization, Text.encodeUtf8 $ "Bearer " <> Credential.accessToken (Credential.credentialAccessToken cred))
              ]
          }
  HTTP.withResponse request mgr handleRegisterClient
  where
    handleRegisterClient response = do
      let status = HTTP.responseStatus response
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      if status `notElem` [HTTP.status200, HTTP.status201]
        then error $ "Register Client failed with status " <> show status <> " and Body " <> show bodyText
        else case Aeson.eitherDecodeStrict bodyText of
          Left e -> error $ "Failed to decode client" <> e
          Right c -> pure c

runListConvs :: HTTP.Manager -> ServerCredential -> Natural -> Maybe ConvId -> IO Convs
runListConvs mgr (ServerCredential server cred) size maybeStart = do
  initialRequest <- HTTP.requestFromURI server
  let qSize = [("size", Just (BSChar8.pack $ show size))]
  let qStart = map (\(ConvId c) -> ("start", Just $ Text.encodeUtf8 c)) (maybeToList maybeStart)
  let request =
        initialRequest
          { method = HTTP.methodGet,
            path = "/conversations",
            queryString = HTTP.renderQuery True (qSize <> qStart),
            requestHeaders =
              [ (HTTP.hContentType, "application/json"),
                (HTTP.hAuthorization, Text.encodeUtf8 $ "Bearer " <> Credential.accessToken (Credential.credentialAccessToken cred))
              ]
          }
  HTTP.withResponse request mgr handleListConvs
  where
    handleListConvs response = do
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      let status = HTTP.responseStatus response
      if status /= HTTP.status200
        then error $ "Register Client failed with status " <> show status <> " and Body " <> show bodyText
        else case Aeson.eitherDecodeStrict bodyText of
          Left e -> error $ "Failed to decode conversations" <> e
          Right t -> pure t

runGetNotifications :: HTTP.Manager -> ServerCredential -> Natural -> ClientId -> NotificationId -> IO (NotificationGap, Notifications)
runGetNotifications mgr (ServerCredential server cred) size (ClientId client) (NotificationId since) = do
  initialRequest <- HTTP.requestFromURI server
  let query =
        [ ("size", Just (BSChar8.pack $ show size)),
          ("since", Just (UUID.toASCIIBytes since)),
          ("client", Just (Text.encodeUtf8 client))
        ]
  let request =
        initialRequest
          { method = HTTP.methodGet,
            path = "/notifications",
            queryString = HTTP.renderQuery True query,
            requestHeaders =
              [ (HTTP.hContentType, "application/json"),
                (HTTP.hAuthorization, Text.encodeUtf8 $ "Bearer " <> Credential.accessToken (Credential.credentialAccessToken cred))
              ]
          }
  HTTP.withResponse request mgr handleNotifications
  where
    handleNotifications response = do
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      let status = HTTP.responseStatus response
      gap <-
        if status == HTTP.status200
          then pure NotificationGapDoesNotExist
          else
            if status == HTTP.status404
              then pure NotificationGapExists
              else error $ "Register Client failed with status " <> show status <> " and Body " <> show bodyText
      case Aeson.eitherDecodeStrict bodyText of
        Left e -> error $ "Failed to decode conversations" <> e
        Right t -> pure (gap, t)

runRegisterWireless :: HTTP.Manager -> Opts.RegisterWirelessOptions -> IO [WireCookie]
runRegisterWireless mgr (Opts.RegisterWirelessOptions server name) = do
  let body = Aeson.object ["name" .= name]
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode body,
            path = "/register",
            requestHeaders = [(HTTP.hContentType, "application/json")]
          }
  HTTP.withResponse request mgr handleResponse
  where
    handleResponse response = do
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      let status = HTTP.responseStatus response
      if status /= HTTP.status201
        then error $ "Registration failed with status " <> show status <> " and Body " <> show bodyText
        else pure $ map WireCookie $ HTTP.destroyCookieJar $ HTTP.responseCookieJar response

runRefreshToken :: HTTP.Manager -> URI -> [WireCookie] -> IO AccessToken
runRefreshToken mgr server cookies = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/access",
            cookieJar = Just $ HTTP.createCookieJar (map unWireCookie cookies)
          }
  HTTP.withResponse request mgr handleResponse
  where
    handleResponse response = do
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      let status = HTTP.responseStatus response
      if status /= HTTP.status200
        then error $ "Refresh token failed with status " <> show status <> " and Body " <> show bodyText
        else case Aeson.decodeStrict bodyText of
          Nothing -> error "Failed to decode access token"
          Just t -> pure t
