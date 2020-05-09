module Wire.CLI.Backend.HTTP where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (method, path, requestBody, requestHeaders)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Polysemy
import Wire.CLI.Backend.Client (NewClient)
import Wire.CLI.Backend.Credential (Credential (..), LoginResponse (..), ServerCredential (ServerCredential), WireCookie (..))
import qualified Wire.CLI.Backend.Credential as Credential
import Wire.CLI.Backend.Effect
import qualified Wire.CLI.Options as Opts

run :: Member (Embed IO) r => Text -> HTTP.Manager -> Sem (Backend ': r) a -> Sem r a
run label mgr = interpret $
  embed . \case
    Login opts -> runLogin label mgr opts
    RegisterClient serverCred client -> runRegisterClient mgr serverCred client

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
      let status = HTTP.responseStatus response
      if status /= HTTP.status200
        then do
          bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
          pure $ LoginFailure $ "Login failed with status " <> Text.pack (show status) <> " and Body " <> Text.pack (show bodyText)
        else do
          bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
          case Aeson.decodeStrict bodyText of
            Nothing -> pure $ LoginFailure "Failed to decode access token"
            Just t -> do
              let c = map WireCookie $ HTTP.destroyCookieJar $ HTTP.responseCookieJar response
              pure $ LoginSuccess $ Credential c t

runRegisterClient :: HTTP.Manager -> ServerCredential -> NewClient -> IO ()
runRegisterClient mgr (ServerCredential server cred) newClient = do
  initialRequest <- HTTP.requestFromURI server
  let request =
        initialRequest
          { method = HTTP.methodPost,
            requestBody = HTTP.RequestBodyLBS $ Aeson.encode newClient,
            path = "/clients",
            requestHeaders =
              [ (HTTP.hContentType, "application/json"),
                (HTTP.hAuthorization, Text.encodeUtf8 $ "Bearer " <> Credential.token (Credential.accessToken cred))
              ]
          }
  HTTP.withResponse request mgr handleRegisterClient
  where
    handleRegisterClient response = do
      let status = HTTP.responseStatus response
      if status `notElem` [HTTP.status200, HTTP.status201]
        then do
          bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
          error $ "Register Client failed with status " <> show status <> " and Body " <> show bodyText
        else pure ()
