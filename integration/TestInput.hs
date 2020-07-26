module TestInput where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Options.Applicative as Opts

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
