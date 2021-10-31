{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry (retrying)
import qualified Control.Retry as Retry
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Id
import Data.Maybe (isNothing, listToMaybe)
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Lens.Family2 ((&), (.~), (^.))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opts
import Polysemy (Embed, Members, Sem, embed, runM)
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader
import qualified Proto.Messages as M
import Shelly (Sh, shelly)
import qualified Shelly
import qualified System.Environment as Env
import qualified System.IO.Temp as Temp
import System.Random (randomRIO)
import Test.Hspec
import qualified Test.Hspec.Core.Runner as Hspec
import TestInput
import qualified Wire.API.Connection as Connection
import qualified Wire.API.User.Search as Search
import Wire.CLI.Store (StoredMessage (smMessage), StoredMessageData (..))
import Data.Qualified

main :: IO ()
main = HTTP.withOpenSSL $ do
  mgr <- HTTP.newManager $ HTTP.opensslManagerSettings sslContext
  cfg <-
    Opts.execParser $
      Opts.info
        (configParser <**> Opts.helper)
        (Opts.fullDesc <> Opts.progDesc "Wire CLI Integration tests")
  Hspec.readConfig Hspec.defaultConfig (hspecArgs cfg)
    >>= Env.withArgs [] . Hspec.runSpec (spec $ TestInput mgr cfg)
    >>= Hspec.evaluateSummary

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: TestInput -> Spec
spec input = do
  describe "WireCLI" $ do
    specify "simple happy path" $
      runM @IO . Reader.runReader input $ do
        (_, user1Dir) <- registerUser
        (user2Name, user2Dir) <- registerUser
        searchAndConnect user1Dir user2Name
        conn <- head <$> getPendingConnections user2Dir
        acceptConn user2Dir conn
        user1Conns <- head <$> getAllConnections user1Dir
        embed $ Connection.ucStatus user1Conns `shouldBe` Connection.Accepted
        conv <- liftIO . assertJust $ qUnqualified <$> Connection.ucConvId conn
        -- Send a lot of messages to ensure cryptobox session management is ok
        verifyMessageTrip conv user1Dir user2Dir "Message 1"
        verifyMessageTrip conv user2Dir user1Dir "Message 2"

        verifyMessageTrip conv user1Dir user2Dir "Message 3"
        verifyMessageTrip conv user1Dir user2Dir "Message 4"

        verifyMessageTrip conv user2Dir user1Dir "Message 5"
        verifyMessageTrip conv user2Dir user1Dir "Message 6"
        verifyMessageTrip conv user2Dir user1Dir "Message 7"
        verifyMessageTrip conv user2Dir user1Dir "Message 8"
        verifyMessageTrip conv user2Dir user1Dir "Message 9"
        verifyMessageTrip conv user2Dir user1Dir "Message 10"

verifyMessageTrip :: Members [Reader TestInput, Embed IO] r => ConvId -> Text -> Text -> Text -> Sem r ()
verifyMessageTrip conv fromDir toDir sentMsg = do
  sendMessage fromDir conv sentMsg
  recievedMsg <- syncUntilMessage toDir conv
  embed $ case smMessage recievedMsg of
    InvalidMessage err ->
      expectationFailure $ "Failed to parse message with: " <> err
    ValidMessage msg ->
      msg ^. #maybe'content `shouldBe` Just (M.GenericMessage'Text (Proto.defMessage & #content .~ sentMsg))

getLastMessage :: Members [Reader TestInput, Embed IO] r => Text -> ConvId -> Sem r (Maybe StoredMessage)
getLastMessage userDir conv = do
  syncNotifications userDir
  msgs <- decodeJSONText =<< cliWithDir userDir ["list-messages", "--conv", idToText conv, "-n", "1"]
  pure $ listToMaybe msgs

syncUntilMessage :: Members [Reader TestInput, Embed IO] r => Text -> ConvId -> Sem r StoredMessage
syncUntilMessage userDir conv = do
  maybeRes <-
    retrying
      (Retry.constantDelay 50000 <> Retry.limitRetries 15)
      (\_ x -> pure $ isNothing x)
      (const $ getLastMessage userDir conv)
  case maybeRes of
    Nothing -> error $ "Search for '" <> show conv <> "' did not yield any results"
    Just m -> pure m

sendMessage :: Members [Reader TestInput, Embed IO] r => Text -> ConvId -> Text -> Sem r ()
sendMessage userDir conv msg = cliWithDir_ userDir ["send-message", "--to", idToText conv, "--message", msg]

registerUser :: Members [Reader TestInput, Embed IO] r => Sem r (Text, Text)
registerUser = do
  Config {..} <- Reader.asks config
  name <- embed $ Text.pack <$> Monad.replicateM 10 (randomRIO ('a', 'z'))
  let email = Text.replace "${random}" name emailTemplate
  systemTempDir <- embed Temp.getCanonicalTemporaryDirectory
  userDir <- embed $ Text.pack <$> Temp.createTempDirectory systemTempDir "wire-cli-integration-test"
  cli_ $
    ["request-activation-code"]
      ++ ["--email", email]
      ++ ["--server", (Text.pack . show) backendURI]
  activationCode <- getActivationCode email
  cliWithDir_ userDir $
    ["register"]
      ++ ["--server", (Text.pack . show) backendURI]
      ++ ["--email", email]
      ++ ["--email-code", activationCode]
      ++ ["--name", name]
      ++ ["--password", "p@ssw0rd"]
  cliWithDir_
    userDir
    ["set-handle", "--handle", name]
  pure (name, userDir)

searchAndConnect :: Members [Reader TestInput, Embed IO] r => Text -> Text -> Sem r ()
searchAndConnect userDir query = do
  userId <- searchUntilFound userDir query
  cliWithDir_ userDir ["connect", "--user-id", idToText userId, "--conv-name", "some-conv"]

acceptConn :: Members [Reader TestInput, Embed IO] r => Text -> Connection.UserConnection -> Sem r ()
acceptConn userDir conn = do
  let to = qUnqualified $ Connection.ucTo conn
  cliWithDir_ userDir ["update-connection", "--to", idToText to, "--status", "accepted"]

searchUntilFound :: Members [Reader TestInput, Embed IO] r => Text -> Text -> Sem r UserId
searchUntilFound userDir query = do
  maybeRes <-
    retrying
      (Retry.constantDelay 50000 <> Retry.limitRetries 15)
      (\_ x -> pure $ isNothing x)
      (const $ search userDir query)
  case maybeRes of
    Nothing -> error $ "Search for '" <> Text.unpack query <> "' did not yield any results"
    Just u -> pure u

-- | Only returns first result, if any
search :: Members [Reader TestInput, Embed IO] r => Text -> Text -> Sem r (Maybe UserId)
search userDir query = do
  searchRes <- decodeJSONText =<< cliWithDir userDir ["search", "--query", query]
  case Search.searchResults searchRes of
    [] -> pure Nothing
    x : _ -> pure . Just . qUnqualified $ Search.contactQualifiedId x

getPendingConnections :: Members [Reader TestInput, Embed IO] r => Text -> Sem r [Connection.UserConnection]
getPendingConnections userDir = do
  syncNotifications userDir
  decodeJSONText =<< cliWithDir userDir ["list-connections", "--status=pending"]

getAllConnections :: Members [Reader TestInput, Embed IO] r => Text -> Sem r [Connection.UserConnection]
getAllConnections userDir = do
  syncNotifications userDir
  decodeJSONText =<< cliWithDir userDir ["list-connections"]

syncNotifications :: Members [Reader TestInput, Embed IO] r => Text -> Sem r ()
syncNotifications userDir =
  cliWithDir_ userDir ["sync-notifications"]

getActivationCode ::
  Members [Embed IO, Reader TestInput] r =>
  -- | Email Id
  Text ->
  Sem r Text
getActivationCode email = do
  mgr <- Reader.asks httpManager
  Config {..} <- Reader.asks config
  initialRequest <- embed @IO $ case backdoor of
    BackdoorNginz UserPassword {..} ->
      HTTP.applyBasicAuth (Text.encodeUtf8 user) (Text.encodeUtf8 password)
        <$> HTTP.requestFromURI backendURI
    BackdoorBrig brigURI -> HTTP.requestFromURI brigURI
  let query = [("email", Just $ Text.encodeUtf8 email)]
  let request =
        initialRequest
          { HTTP.path = "/i/users/activation-code",
            HTTP.queryString = HTTP.renderQuery True query
          }
  embed $ HTTP.withResponse request mgr handleResponse
  where
    handleResponse response = do
      bodyText <- BS.concat <$> HTTP.brConsume (HTTP.responseBody response)
      let status = HTTP.responseStatus response
      if status /= HTTP.status200
        then error $ "get activation-code failed with status " <> show status <> " and Body " <> show bodyText
        else case Aeson.eitherDecodeStrict bodyText of
          Left e -> error $ "Failed to decode result for activation-code with error: " <> e
          Right (ActivationCode t) -> pure t

cliWithDir_ :: Members [Reader TestInput, Embed IO] r => Text -> [Text] -> Sem r ()
cliWithDir_ userDir args = cli_ $ ["--file-store-path", userDir] <> args

cliWithDir :: Members [Reader TestInput, Embed IO] r => Text -> [Text] -> Sem r Text
cliWithDir userDir args = cli $ ["--file-store-path", userDir] <> args

cli_ :: Members [Reader TestInput, Embed IO] r => [Text] -> Sem r ()
cli_ args = do
  Config {..} <- Reader.asks config
  executeShelly $ Shelly.run_ wireCliPath args

cli :: Members [Reader TestInput, Embed IO] r => [Text] -> Sem r Text
cli args = do
  Config {..} <- Reader.asks config
  executeShelly $ Shelly.run wireCliPath args

executeShelly :: Members [Reader TestInput, Embed IO] r => Sh a -> Sem r a
executeShelly action = do
  Config {..} <- Reader.asks config
  shelly $
    Shelly.print_commands verbose $
      Shelly.print_stdout verbose action

decodeJSONText :: (MonadIO m, FromJSON a) => Text -> m a
decodeJSONText = liftIO . assertRight . Aeson.eitherDecodeStrict . Text.encodeUtf8

assertRight :: Show a => Either a b -> IO b
assertRight (Left x) = expectationFailure ("expected Right, got Left: " <> show x) >> error "Impossible!"
assertRight (Right x) = pure x

assertJust :: Show a => Maybe a -> IO a
assertJust Nothing = expectationFailure "expected Just, got Nothing" >> error "Impossible!"
assertJust (Just x) = pure x

sslContext :: IO SSL.SSLContext
sslContext = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextLoadSystemCerts ctx
  pure ctx

newtype ActivationCode = ActivationCode Text

instance FromJSON ActivationCode where
  parseJSON = Aeson.withObject "ActivationCode" $ \o -> ActivationCode <$> o .: "code"
