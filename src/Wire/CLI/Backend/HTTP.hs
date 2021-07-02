{-# LANGUAGE MultiWayIf #-}

module Wire.CLI.Backend.HTTP where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
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
import Polysemy.Error (Error)
import qualified Polysemy.Error as Polysemy
import Wire.CLI.Backend.Client (Client, ClientId (..), NewClient)
import Wire.CLI.Backend.Connection (ConnectionList, ConnectionRequest)
import qualified Wire.CLI.Backend.Connection as Connection
import Wire.CLI.Backend.Conv (ConvId (..), Convs)
import Wire.CLI.Backend.Credential (AccessToken (..), Credential (..), LoginResponse (..), ServerCredential (ServerCredential), WireCookie (..))
import qualified Wire.CLI.Backend.Credential as Credential
import Wire.CLI.Backend.Effect
import Wire.CLI.Backend.Message (NewOtrMessage, PrekeyBundles, SendOtrMessageResponse (..), UserClients)
import Wire.CLI.Backend.Notification (NotificationGap (..), NotificationId (..), Notifications)
import Wire.CLI.Backend.Search (SearchResults (..))
import Wire.CLI.Backend.User (Handle, UserId (..))
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Options as Opts

-- TODO: Get rid of all the 'error' calls
run :: Members [Embed IO, Error WireCLIError] r => Text -> HTTP.Manager -> Sem (Backend ': r) a -> Sem r a
run label mgr =
  interpret $
    catchHTTPException . \case
      Login opts -> runLogin label mgr opts
      RegisterClient serverCred client -> runRegisterClient mgr serverCred client
      ListConvs serverCred size start -> runListConvs mgr serverCred size start
      GetNotifications serverCred size since client -> runGetNotifications mgr serverCred size since client
      RegisterWireless opts -> runRegisterWireless mgr opts
      RefreshToken serverCred cookies -> runRefreshToken mgr serverCred cookies
      Search serverCred opts -> runSearch mgr serverCred opts
      RequestActivationCode opts -> runRequestActivationCode mgr opts
      Register opts -> runRegister mgr opts
      SetHandle serverCred handle -> runSetHandle mgr serverCred handle
      GetConnections serverCred size start -> runGetConnections mgr serverCred size start
      UpdateConnection serverCred uid rel -> runUpdateConnection mgr serverCred uid rel
      Connect serverCred cr -> runConnect mgr serverCred cr
      GetPrekeyBundles serverCred userClients -> runGetPrekeyBundles mgr serverCred userClients
      SendOtrMessage serverCred conv msg -> runSendOtrMessage mgr serverCred conv msg

catchHTTPException :: Members [Error WireCLIError, Embed IO] r => IO a -> Sem r a
catchHTTPException action = do
  eithRes <-
    embed $
      Exception.catches
        (Right <$> action)
        [ Exception.Handler $ pure . Left . WireCLIError.HttpException,
          Exception.Handler $ pure . Left
        ]
  either Polysemy.throw pure eithRes

runLogin :: Text -> HTTP.Manager -> Opts.LoginOptions -> IO LoginResponse
runLogin label mgr (Opts.LoginOptions server identity password) = do
  let body =
        Aeson.object
          [ "password" .= password,
            "label" .= label,
            case identity of
              Opts.LoginEmail e -> "email" .= e
              Opts.LoginHandle h -> "handle" .= h
          ]
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode body,
            path = "/login",
            requestHeaders = [contentTypeJSON]
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
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr handleRegisterClient
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
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr (expect200JSON "list conversations")

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
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr handleNotifications
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

runRefreshToken :: HTTP.Manager -> URI -> [WireCookie] -> IO AccessToken
runRefreshToken mgr server cookies = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/access",
            cookieJar = Just $ HTTP.createCookieJar (map unWireCookie cookies)
          }
  HTTP.withResponse request mgr (expect200JSON "refresh token")

runSearch :: HTTP.Manager -> ServerCredential -> Opts.SearchOptions -> IO SearchResults
runSearch mgr (ServerCredential server cred) (Opts.SearchOptions q size) = do
  initialRequest <- HTTP.requestFromURI server
  let query =
        [ ("size", Just (BSChar8.pack $ show size)),
          ("q", Just (Text.encodeUtf8 q))
        ]
  let request =
        initialRequest
          { method = HTTP.methodGet,
            path = "/search/contacts",
            queryString = HTTP.renderQuery True query,
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr (expect200JSON "search")

runRequestActivationCode :: HTTP.Manager -> Opts.RequestActivationCodeOptions -> IO ()
runRequestActivationCode mgr (Opts.RequestActivationCodeOptions server email locale) = do
  initialRequest <- HTTP.requestFromURI server
  let body = Aeson.object ["email" .= email, "locale" .= locale]
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/activate/send",
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode body,
            requestHeaders = [contentTypeJSON]
          }
  HTTP.withResponse request mgr (Monad.void . expect200 "request activation code")

runRegisterWireless :: HTTP.Manager -> Opts.RegisterWirelessOptions -> IO [WireCookie]
runRegisterWireless mgr (Opts.RegisterWirelessOptions server name) = do
  let body = Aeson.object ["name" .= name]
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode body,
            path = "/register",
            requestHeaders = [contentTypeJSON]
          }
  HTTP.withResponse request mgr expect201Cookie

runRegister :: HTTP.Manager -> Opts.RegisterOptions -> IO [WireCookie]
runRegister mgr (Opts.RegisterOptions server name email emailCode password) = do
  initialRequest <- HTTP.requestFromURI server
  let body =
        Aeson.object
          [ "email" .= email,
            "name" .= name,
            "email_code" .= emailCode,
            "password" .= password
          ]
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/register",
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode body,
            requestHeaders = [contentTypeJSON]
          }
  HTTP.withResponse request mgr expect201Cookie

runSetHandle :: HTTP.Manager -> ServerCredential -> Handle -> IO ()
runSetHandle mgr (ServerCredential server cred) handle = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPut,
            path = "/self/handle",
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode $ Aeson.object ["handle" .= handle],
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr (Monad.void . expect200 "set-handle")

runGetConnections :: HTTP.Manager -> ServerCredential -> Natural -> Maybe UserId -> IO ConnectionList
runGetConnections mgr (ServerCredential server cred) size maybeStart = do
  initialRequest <- HTTP.requestFromURI server
  let qStart = map (\(UserId u) -> ("start", Just $ Text.encodeUtf8 u)) (maybeToList maybeStart)
  let query = [("size", Just (BSChar8.pack $ show size))] <> qStart
  let request =
        initialRequest
          { method = HTTP.methodGet,
            path = "/connections",
            queryString = HTTP.renderQuery True query
          }
  withAuthenticatedResponse cred request mgr (expect200JSON "connections")

runConnect :: HTTP.Manager -> ServerCredential -> ConnectionRequest -> IO ()
runConnect mgr (ServerCredential server cred) cr = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/connections",
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode cr,
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr (Monad.void . expect201 "connect")

runUpdateConnection :: HTTP.Manager -> ServerCredential -> UserId -> Connection.Relation -> IO ()
runUpdateConnection mgr (ServerCredential server cred) (UserId uid) rel = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPut,
            path = "/connections/" <> Text.encodeUtf8 uid,
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode (Aeson.object ["status" .= rel]),
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr (Monad.void . expectOneOf [HTTP.status200, HTTP.status204] "update-connection")

runSendOtrMessage :: HTTP.Manager -> ServerCredential -> ConvId -> NewOtrMessage -> IO SendOtrMessageResponse
runSendOtrMessage mgr (ServerCredential server cred) (ConvId conv) msg = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/conversations/" <> Text.encodeUtf8 conv <> "/otr/messages",
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode msg,
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr handleResponse
  where
    handleResponse response = do
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      let status = HTTP.responseStatus response
      if
          | status == HTTP.status201 -> pure OtrMessageResponseSuccess
          | status == HTTP.status412 -> OtrMessageResponseClientMismatch <$> expectJSON "otr-response-client-mismatch" bodyText
          | otherwise -> error ("send-otr-message failed with status " <> show status <> " and Body " <> show bodyText)

runGetPrekeyBundles :: HTTP.Manager -> ServerCredential -> UserClients -> IO PrekeyBundles
runGetPrekeyBundles mgr (ServerCredential server cred) userClients = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/users/prekeys",
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode userClients,
            requestHeaders = [contentTypeJSON]
          }
  withAuthenticatedResponse cred request mgr (expect200JSON "get-prekey-bundles")

expectJSON :: Aeson.FromJSON a => String -> BSChar8.ByteString -> IO a
expectJSON name body = case Aeson.eitherDecodeStrict body of
  Left e -> error $ "Failed to decode result for " <> name <> ": " <> e
  Right t -> pure t

expect200JSON :: Aeson.FromJSON a => String -> HTTP.Response HTTP.BodyReader -> IO a
expect200JSON name response = expect200 name response >>= expectJSON name

expect200 :: String -> HTTP.Response HTTP.BodyReader -> IO BSChar8.ByteString
expect200 name response = do
  bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
  let status = HTTP.responseStatus response
  Monad.when (status /= HTTP.status200) $
    error (name <> " failed with status " <> show status <> " and Body " <> show bodyText)
  pure bodyText

expect201 :: String -> HTTP.Response HTTP.BodyReader -> IO BSChar8.ByteString
expect201 name response = do
  bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
  let status = HTTP.responseStatus response
  Monad.when (status /= HTTP.status201) $
    error (name <> " failed with status " <> show status <> " and Body " <> show bodyText)
  pure bodyText

expect201Cookie :: HTTP.Response HTTP.BodyReader -> IO [WireCookie]
expect201Cookie response = do
  bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
  let status = HTTP.responseStatus response
  if status /= HTTP.status201
    then error $ "Registration failed with status " <> show status <> " and Body " <> show bodyText
    else pure $ map WireCookie $ HTTP.destroyCookieJar $ HTTP.responseCookieJar response

expectOneOf :: [HTTP.Status] -> String -> HTTP.Response HTTP.BodyReader -> IO BSChar8.ByteString
expectOneOf codes name response = do
  bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
  let status = HTTP.responseStatus response
  Monad.when (status `notElem` codes) $
    error (name <> " failed with status " <> show status <> " and Body " <> show bodyText)
  pure bodyText

mkAuthHeader :: Credential -> HTTP.Header
mkAuthHeader cred =
  ( HTTP.hAuthorization,
    Text.encodeUtf8 $ "Bearer " <> Credential.accessToken (Credential.credentialAccessToken cred)
  )

contentTypeJSON :: HTTP.Header
contentTypeJSON = (HTTP.hContentType, "application/json")

withAuthenticatedResponse :: forall a. Credential -> HTTP.Request -> HTTP.Manager -> (HTTP.Response HTTP.BodyReader -> IO a) -> IO a
withAuthenticatedResponse cred req mgr handler = do
  let reqWithAuth = req {HTTP.requestHeaders = mkAuthHeader cred : HTTP.requestHeaders req}
  HTTP.withResponse reqWithAuth mgr $ \res ->
    if HTTP.responseStatus res == HTTP.status401
      then Exception.throw WireCLIError.Http401
      else handler res
