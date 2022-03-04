module Wire.CLI.Backend.HTTP where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UnagiNB
import qualified Control.Exception as Exception
import Control.Monad (void)
import qualified Control.Monad as Monad
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Conversion (toByteString')
import Data.Handle (Handle)
import Data.Id (ClientId (ClientId), ConvId, UserId)
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Network.HTTP.Client (cookieJar, method, path, queryString, requestBody, requestHeaders)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (readPositiveInt)
import qualified Network.HTTP.Client.WebSockets as WS
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import Polysemy (Embed, Members, Sem, embed, interpret)
import Polysemy.Error (Error)
import qualified Polysemy.Error as Polysemy
import qualified Servant.API as Servant
import qualified Servant.Client as Servant
import Wire.API.Connection
import Wire.API.Conversation (Conversation, ConversationList)
import Wire.API.Message
import Wire.API.Routes.MultiTablePaging
import qualified Wire.API.Routes.Public.Brig as Brig
import Wire.API.ServantProto
import Wire.API.User (SelfProfile, UserProfile)
import Wire.API.User.Client
import Wire.API.User.Search
import qualified Wire.CLI.Backend.API as API
import Wire.CLI.Backend.Credential (Credential (..), LoginResponse (..), ServerCredential (ServerCredential), WireCookie (..))
import qualified Wire.CLI.Backend.Credential as Credential
import Wire.CLI.Backend.Effect (Backend (..))
import Wire.CLI.Backend.Notification (NotificationGap (..), NotificationId (..), Notifications, WSNotification)
import qualified Wire.CLI.Backend.Notification as Notification
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
      Connect serverCred quid -> runConnect mgr serverCred quid
      GetPrekeyBundles serverCred qUserClients -> runGetPrekeyBundles mgr serverCred qUserClients
      SendOtrMessage serverCred conv msg -> runSendOtrMessage mgr serverCred conv msg
      GetUser serverCred uid -> runGetUser mgr serverCred uid
      GetSelf serverCred -> runGetSelf mgr serverCred
      WatchNotifications serverCred mClientId -> runWatchNotifications mgr serverCred mClientId

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

-- Not servantified
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
            queryString = HTTP.renderQuery True [("persist", Just "true")],
            requestHeaders = [contentTypeJSON]
          }
  HTTP.withResponse request mgr readCredential >>= \case
    CRSuccess cred ->
      pure $ LoginSuccess cred
    CRWrongStatus status bodyText ->
      pure $ LoginFailure $ "Login failed with status " <> Text.pack (show status) <> " and Body " <> Text.pack (show bodyText)
    CRInvalidBody _ ->
      pure $ LoginFailure "Failed to decode access token"

-- Not servantified
runRefreshToken :: HTTP.Manager -> URI -> [WireCookie] -> IO Credential
runRefreshToken mgr server cookies = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            path = "/access",
            cookieJar = Just $ HTTP.createCookieJar (map unWireCookie cookies)
          }
  HTTP.withResponse request mgr readCredential >>= \case
    CRSuccess cred -> pure cred
    CRWrongStatus status body ->
      error $ "Failed to refresh token with status " <> show status <> " and Body " <> show body
    CRInvalidBody body ->
      error $ "Failed to parse token body: " <> show body

readCredential :: HTTP.Response HTTP.BodyReader -> IO CredentialResponse
readCredential response = do
  bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
  let status = HTTP.responseStatus response
  if status /= HTTP.status200
    then pure $ CRWrongStatus status bodyText
    else case Aeson.decodeStrict bodyText of
      Nothing -> pure $ CRInvalidBody bodyText
      Just t -> do
        let c = map WireCookie $ HTTP.destroyCookieJar $ HTTP.responseCookieJar response
        pure $ CRSuccess $ Credential c t

runRegisterClient :: HTTP.Manager -> ServerCredential -> NewClient -> IO Client
runRegisterClient mgr serverCred nc = do
  runServantClientWithServerCred mgr serverCred $
    \token -> Servant.getResponse <$> Brig.addClient API.brigClient token Nothing nc

runListConvs :: HTTP.Manager -> ServerCredential -> Maybe (Range 1 500 Int32) -> Maybe ConvId -> IO (ConversationList Conversation)
runListConvs mgr serverCred maybeSize maybeStart = do
  runServantClientWithServerCred mgr serverCred $
    \token -> API.galleyClient @"get-conversations" token Nothing maybeStart maybeSize

-- Not servantified
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

data CredentialResponse
  = CRWrongStatus HTTP.Status BSChar8.ByteString
  | CRInvalidBody BSChar8.ByteString
  | CRSuccess Credential

runSearch :: HTTP.Manager -> ServerCredential -> Opts.SearchOptions -> IO (SearchResult Contact)
runSearch mgr serverCred (Opts.SearchOptions q mDomain size) = do
  runServantClientWithServerCred mgr serverCred $
    \token -> Brig.searchContacts API.brigClient token q mDomain (Just size)

-- Not servantified
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

-- Not servantified
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

-- Not servantified
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

-- Not servantified
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

runGetConnections :: HTTP.Manager -> ServerCredential -> Maybe (Range 1 500 Int32) -> Maybe ConnectionPagingState -> IO ConnectionsPage
runGetConnections mgr serverCred msize state = do
  let -- TODO: Don't hardcode 100, use the 'def' field from 'GetMultiTablePageRequest'
      size = fromMaybe (toRange (Proxy @100)) msize
      req = GetMultiTablePageRequest size (hackPageReq <$> state)
  runServantClientWithServerCred mgr serverCred $
    \token -> Brig.listConnections API.brigClient token req
  where
    -- This is required because 'ConnectionPagingState' and
    -- 'ListConnectionsRequestPaginated' have different symbols to describe what
    -- they are. TODO: fix this in wire-api.
    hackPageReq :: MultiTablePagingState n1 t -> MultiTablePagingState n2 t
    hackPageReq (MultiTablePagingState size' state') = MultiTablePagingState size' state'

runConnect :: HTTP.Manager -> ServerCredential -> Qualified UserId -> IO ()
runConnect mgr serverCred quid = do
  runServantClientWithServerCred mgr serverCred $
    \token -> void $ Brig.createConnection API.brigClient token quid

runUpdateConnection :: HTTP.Manager -> ServerCredential -> Qualified UserId -> Relation -> IO ()
runUpdateConnection mgr serverCred uid rel = do
  runServantClientWithServerCred mgr serverCred $
    \token -> void $ Brig.updateConnection API.brigClient token uid (ConnectionUpdate rel)

runSendOtrMessage :: HTTP.Manager -> ServerCredential -> Qualified ConvId -> QualifiedNewOtrMessage -> IO (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus)
runSendOtrMessage mgr serverCred conv msg = do
  runServantClientWithServerCred mgr serverCred $
    \token -> API.galleyClient @"post-proteus-message" token conv (RawProto (toProto msg) msg)

runGetPrekeyBundles :: HTTP.Manager -> ServerCredential -> QualifiedUserClients -> IO QualifiedUserClientPrekeyMap
runGetPrekeyBundles mgr serverCred qUserClients = do
  runServantClientWithServerCred mgr serverCred $
    \token -> Brig.getMultiUserPrekeyBundleQualified API.brigClient token qUserClients

runGetUser :: HTTP.Manager -> ServerCredential -> Qualified UserId -> IO (Maybe UserProfile)
runGetUser mgr serverCred quid = do
  runServantClientWithServerCred mgr serverCred $
    \token -> Brig.getUserQualified API.brigClient token quid

runGetSelf :: HTTP.Manager -> ServerCredential -> IO SelfProfile
runGetSelf mgr serverCred = do
  runServantClientWithServerCred mgr serverCred $
    Brig.getSelf API.brigClient

runWatchNotifications :: HTTP.Manager -> ServerCredential -> Maybe ClientId -> IO (UnagiNB.OutChan WSNotification)
runWatchNotifications mgr (ServerCredential server cred) mClientId = do
  initialRequest <- HTTP.requestFromURI server

  let request =
        initialRequest
          { method = HTTP.methodGet,
            path = "/await",
            queryString = HTTP.renderQuery True [("clientId", Just (toByteString' c)) | c <- maybeToList mClientId],
            requestHeaders = [mkAuthHeader cred]
          }
  (inChan, outChan) <- UnagiNB.newChan
  -- TODO: Capture this thread id to allow closing
  _ <-
    forkIO . WS.runClientWithRequest mgr request WS.defaultConnectionOptions $
      \conn -> do
        Notification.wsApp inChan conn
  pure outChan

-- * Utils

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
  HTTP.withResponse reqWithAuth mgr $ \res -> do
    if HTTP.responseStatus res == HTTP.status401
      then Exception.throw WireCLIError.Http401
      else handler res

-- * Servant Stuff

newtype InvalidHttpURI = InvalidHTTPURI URI
  deriving (Show)

instance Exception.Exception InvalidHttpURI

-- TODO: Test, explode better
baseUrlFromURI :: URI -> IO Servant.BaseUrl
baseUrlFromURI u = do
  (sc, defPort) <- case URI.uriScheme u of
    "http:" -> pure (Servant.Http, 80)
    "https:" -> pure (Servant.Https, 443)
    _ -> explode
  hs <- maybe explode (pure . URI.uriRegName) $ URI.uriAuthority u
  pr <- case URI.uriPort <$> URI.uriAuthority u of
    Nothing -> pure defPort
    Just "" -> pure defPort
    Just (':' : rest) ->
      maybe explode pure $ readPositiveInt rest
    _ -> explode
  pure $ Servant.BaseUrl sc hs pr (URI.uriPath u)
  where
    explode :: forall a. IO a
    explode = Exception.throwIO $ InvalidHTTPURI u

-- | TODO: Automatically refresh token and retry
runServantClientWithServerCred ::
  HTTP.Manager ->
  ServerCredential ->
  -- | Users must ensure that only exactly one call to the API is made in this
  -- function, if multiple calls are made and one of the call requires a token
  -- refresh, all the calls will get retried.
  (Text -> Servant.ClientM a) ->
  IO a
runServantClientWithServerCred mgr (ServerCredential server cred) f = do
  clientEnv <- Servant.mkClientEnv mgr <$> baseUrlFromURI server
  let token = Credential.accessToken (Credential.credentialAccessToken cred)
  Servant.runClientM (f token) clientEnv >>= \case
    Left err@(Servant.FailureResponse _ res) ->
      if Servant.responseStatusCode res == HTTP.status401
        then Exception.throw WireCLIError.Http401
        else Exception.throw err
    Left err -> Exception.throw err
    Right x -> pure x
