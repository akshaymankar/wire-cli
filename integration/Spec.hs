{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry (retrying)
import qualified Control.Retry as Retry
import Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import qualified Options.Applicative as Opts
import Options.Applicative ((<**>))
import Polysemy
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader
import qualified Shelly
import Shelly (shelly)
import qualified System.Environment as Env
import qualified System.IO.Temp as Temp
import System.Random (randomRIO)
import Test.Hspec
import qualified Test.Hspec.Core.Runner as Hspec
import Wire.CLI.Backend.Connection (Connection)
import qualified Wire.CLI.Backend.Connection as Connection
import qualified Wire.CLI.Backend.Search as Search
import Wire.CLI.Backend.User (UserId (..))

data UserPassword = UserPassword
  { user :: Text,
    password :: Text
  }

data Backdoor
  = BackdoorNginz UserPassword
  | BackdoorBrig URI

data Config = Config
  { wireCliPath :: FilePath,
    backendURI :: URI,
    backdoor :: Backdoor,
    -- | Format: foo+${random}@example.com
    emailTemplate :: Text,
    hspecArgs :: [String]
  }

uriOption :: Opts.Mod Opts.OptionFields URI -> Opts.Parser URI
uriOption = Opts.option (Opts.maybeReader URI.parseAbsoluteURI)

userPasswordParser :: String -> Opts.Parser UserPassword
userPasswordParser prefix =
  UserPassword
    <$> Opts.strOption (Opts.long (prefix <> "-user") <> Opts.metavar "USER")
    <*> Opts.strOption (Opts.long (prefix <> "-password") <> Opts.metavar "PASSWORD")

backdoorParser :: Opts.Parser Backdoor
backdoorParser = nginz <|> brig
  where
    nginz :: Opts.Parser Backdoor
    nginz = BackdoorNginz <$> userPasswordParser "backdoor-nginz"
    brig :: Opts.Parser Backdoor
    brig = BackdoorBrig <$> uriOption (Opts.long "backdoor-brig-uri")

configParser :: Opts.Parser Config
configParser =
  Config
    <$> Opts.strOption (Opts.long "wire-cli-path" <> Opts.metavar "PATH")
    <*> uriOption (Opts.long "backend-uri")
    <*> backdoorParser
    <*> Opts.strOption (Opts.long "email-template")
    <*> Opts.many (Opts.strArgument (Opts.metavar "ARG"))

data TestInput = TestInput
  { httpManager :: HTTP.Manager,
    config :: Config
  }

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}

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

spec :: TestInput -> Spec
spec input = do
  let Config {..} = config input
  describe "WireCLI" $ do
    specify "simple happy path" $ runM @IO . Reader.runReader input $ do
      (_, user1Dir) <- registerUser
      (user2Name, user2Dir) <- registerUser
      searchAndConnect user1Dir user2Name "Yo"
      user2Conns <- getPendingConnections user2Dir
      embed $ Connection.connectionMessage (head user2Conns) `shouldBe` Just "Yo"

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
  pure (name, userDir)

searchAndConnect :: Members [Reader TestInput, Embed IO] r => Text -> Text -> Text -> Sem r ()
searchAndConnect userDir query msg = do
  UserId userId <- searchUntilFound userDir query
  cliWithDir_ userDir ["connect", "--user-id", userId, "--conv-name", "some-conv", "--message", msg]

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
  case Search.searchResultsDocuments searchRes of
    [] -> pure Nothing
    x : _ -> pure . Just $ Search.searchResultId x

getPendingConnections :: Members [Reader TestInput, Embed IO] r => Text -> Sem r [Connection]
getPendingConnections userDir = do
  cliWithDir_ userDir ["sync-notifications"]
  decodeJSONText =<< cliWithDir userDir ["list-connections", "--status=pending"]

getActivationCode ::
  Members [Embed IO, Reader TestInput] r =>
  -- | Email Id
  Text ->
  Sem r Text
getActivationCode email = do
  mgr <- Reader.asks httpManager
  Config {..} <- Reader.asks config
  initialRequest <- embed @IO $ case backdoor of
    BackdoorNginz (UserPassword {..}) ->
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
  shelly $ Shelly.run_ wireCliPath args

cli :: Members [Reader TestInput, Embed IO] r => [Text] -> Sem r Text
cli args = do
  Config {..} <- Reader.asks config
  shelly $ Shelly.run wireCliPath args

decodeJSONText :: (MonadIO m, FromJSON a) => Text -> m a
decodeJSONText = liftIO . assertRight . Aeson.eitherDecodeStrict . Text.encodeUtf8

assertRight :: Show a => Either a b -> IO b
assertRight (Left x) = expectationFailure ("expected Right, got Left: " <> show x) >> error "Impossible!"
assertRight (Right x) = pure x

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
