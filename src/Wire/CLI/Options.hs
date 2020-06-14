module Wire.CLI.Options where

import Data.Text
import Network.URI (URI)
import qualified Network.URI as URI
import Options.Applicative
import Wire.CLI.Backend.CommonTypes (Name (..))
import Wire.CLI.Backend.Conv (Conv)
import Wire.CLI.Backend.Search (SearchResults)
import Wire.CLI.Backend.User (Email (..), Handle (..))

newtype StoreConfig = StoreConfig {baseDir :: FilePath}

newtype Config = Config {storeConfig :: StoreConfig}

data RunConfig m = RunConfig {config :: Config, cmd :: Command m}

data Command m
  = Login LoginOptions
  | Logout
  | SyncConvs
  | ListConvs ([Conv] -> m ())
  | RegisterWireless RegisterWirelessOptions
  | RequestActivationCode RequestActivationCodeOptions
  | Register RegisterOptions
  | Search SearchOptions (SearchResults -> m ())
  | SyncNotifications

data Handlers m = Handlers
  { listConvHandler :: [Conv] -> m (),
    searchHandler :: SearchResults -> m ()
  }

data LoginOptions = LoginOptions
  { loginServer :: URI,
    loginHandle :: Text,
    loginPassword :: Text
  }
  deriving (Eq, Show)

data RegisterWirelessOptions = RegisterWirelessOptions
  { registerWirelessServer :: URI,
    regsiterWirelessName :: Name
  }
  deriving (Eq, Show)

data RegisterOptions = RegisterOptions
  { registerServer :: URI,
    registerName :: Name,
    registerEmail :: Email,
    registerEmailCode :: Text,
    registerPassword :: Maybe Text,
    registerHandle :: Maybe Handle
  }
  deriving (Eq, Show)

data RequestActivationCodeOptions = RequestActivationCodeOptions
  { requestActivationServer :: URI,
    requestActivationEmail :: Email,
    requestActivationLocale :: Text
  }
  deriving (Eq, Show)

data SearchOptions = SearchOptions
  { searchOptionQuery :: Text,
    -- | Cannot be larger than 100
    searchOptionMax :: Word
  }

runConfigParser :: Handlers m -> Parser (RunConfig m)
runConfigParser h = RunConfig <$> fmap Config parseStoreConfig <*> commandParser h

parseStoreConfig :: Parser StoreConfig
parseStoreConfig =
  StoreConfig
    <$> strOption
      ( long "file-store-path"
          <> value "/tmp"
          <> help "base directory for client state"
          <> showDefault
      )

commandParser :: Handlers m -> Parser (Command m)
commandParser h =
  subparser $
    command "login" (info (loginParser <**> helper) (progDesc "login to wire"))
      <> command "logout" (info (logoutParser <**> helper) (progDesc "logout of wire"))
      <> command "sync-convs" (info (pure SyncConvs <**> helper) (progDesc "synchronise conversations with the server"))
      <> command "list-convs" (info (pure (ListConvs (listConvHandler h)) <**> helper) (progDesc "list conversations (doesn't fetch them from server)"))
      <> command "sync-notifications" (info (pure SyncNotifications <**> helper) (progDesc "synchronise notifications with the server"))
      <> command "register-wireless" (info (registerWirelessParser <**> helper) (progDesc "register as an anonymous user"))
      <> command "register" (info (registerParser <**> helper) (progDesc "register an account with email"))
      <> command "request-activation-code" (info (requestActivationParser <**> helper) (progDesc "request an activation code for registration"))
      <> command "search" (info (searchParser h <**> helper) (progDesc "search for a user"))

requestActivationParser :: Parser (Command m)
requestActivationParser =
  RequestActivationCode
    <$> ( RequestActivationCodeOptions
            <$> serverParser
            <*> emailParser
            <*> strOption
              ( long "locale"
                  <> value "en-US"
                  <> help "locale to select language for email"
                  <> showDefault
              )
        )

registerParser :: Parser (Command m)
registerParser =
  Register
    <$> ( RegisterOptions
            <$> serverParser
            <*> nameParser
            <*> emailParser
            <*> strOption (long "email-code" <> help "verification code, sent by email using `request-activation-code`")
            <*> optional (strOption (long "password" <> help "password for logging in"))
            <*> optional (Handle <$> strOption (long "username" <> help "also called 'handle'"))
        )

emailParser :: Parser Email
emailParser = Email <$> strOption (long "email" <> help "email address")

loginParser :: Parser (Command m)
loginParser =
  Login
    <$> ( LoginOptions
            <$> serverParser
            <*> strOption (long "username" <> help "username to login as")
            <*> strOption (long "password" <> help "password for the user")
        )

serverParser :: Parser URI
serverParser = uriOption (long "server" <> help "server address")

logoutParser :: Parser (Command m)
logoutParser = pure Logout

registerWirelessParser :: Parser (Command m)
registerWirelessParser =
  RegisterWireless
    <$> ( RegisterWirelessOptions
            <$> serverParser
            <*> nameParser
        )

nameParser :: Parser Name
nameParser = Name <$> strOption (long "name" <> help "name of user, doesn't have to be unique")

searchParser :: Handlers m -> Parser (Command m)
searchParser h =
  Search
    <$> ( SearchOptions
            <$> strOption (long "query" <> short 'q' <> help "search query")
            <*> option auto (long "max" <> value 10 <> help "maximum number of events to return" <> showDefault)
        )
    <*> pure (searchHandler h)

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option (maybeReader URI.parseURI)

readOptions :: Handlers m -> IO (RunConfig m)
readOptions h =
  execParser $
    info
      (runConfigParser h <**> helper)
      (fullDesc <> progDesc "CLI for Wire")
