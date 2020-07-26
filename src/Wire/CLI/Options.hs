module Wire.CLI.Options where

import Data.Text
import Network.URI (URI)
import qualified Network.URI as URI
import Options.Applicative
import Wire.CLI.Backend.CommonTypes (Name (..))
import Wire.CLI.Backend.Connection (Connection, ConnectionMessage (..), ConnectionRequest (..))
import qualified Wire.CLI.Backend.Connection as Connection
import Wire.CLI.Backend.Conv (Conv)
import Wire.CLI.Backend.Search (SearchResults)
import Wire.CLI.Backend.User (Email (..), Handle (..), UserId (..))

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
  | SetHandle Handle
  | Search SearchOptions (SearchResults -> m ())
  | SyncNotifications
  | SyncConnections
  | ListConnections ListConnsOptions ([Connection] -> m ())
  | UpdateConnection UpdateConnOptions
  | Connect ConnectionRequest

data Handlers m = Handlers
  { listConvHandler :: [Conv] -> m (),
    searchHandler :: SearchResults -> m (),
    listConnHandler :: [Connection] -> m ()
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
    registerPassword :: Maybe Text
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

newtype ListConnsOptions = ListConnsOptions
  { status :: Maybe Connection.Relation
  }
  deriving (Eq, Show)

data UpdateConnOptions = UpdateConnOptions
  { updateConnTo :: UserId,
    updateConnStatus :: Connection.Relation
  }
  deriving (Eq, Show)

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
      <> command "set-handle" (info (setHandleParser <**> helper) (progDesc "set handle for the account (also called 'username')"))
      <> command "request-activation-code" (info (requestActivationParser <**> helper) (progDesc "request an activation code for registration"))
      <> command "search" (info (searchParser h <**> helper) (progDesc "search for a user"))
      <> command "sync-connections" (info (pure SyncConnections <**> helper) (progDesc "synchronise connections with the server, only necessary if something went wrong with notifications"))
      <> command "list-connections" (info (listConnsParser h <**> helper) (progDesc "list connections"))
      <> command "update-connection" (info (updateConnParser <**> helper) (progDesc "update connection"))
      <> command "connect" (info (connectParser <**> helper) (progDesc "connect with a user"))

setHandleParser :: Parser (Command m)
setHandleParser =
  SetHandle
    <$> (Handle <$> strOption (long "handle" <> help "handle for the account"))

connectParser :: Parser (Command m)
connectParser =
  Connect
    <$> ( ConnectionRequest
            <$> (UserId <$> strOption (long "user-id" <> help "user id of the user to connect with"))
            <*> strOption (long "conv-name" <> help "name of the conversation")
            <*> (ConnectionMessage <$> strOption (long "message" <> help "connection message"))
        )

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

listConnsParser :: Handlers m -> Parser (Command m)
listConnsParser h =
  ListConnections
    <$> ( ListConnsOptions
            <$> optional (option readConnRelation (long "status"))
        )
    <*> pure (listConnHandler h)

updateConnParser :: Parser (Command m)
updateConnParser =
  UpdateConnection
    <$> ( UpdateConnOptions
            <$> (UserId <$> strOption (long "to" <> help "user id of the other user"))
            <*> option readConnRelation (long "status" <> help "one of: accepted, blocked, pending, ignored, sent, cancelled")
        )

readConnRelation :: ReadM Connection.Relation
readConnRelation = maybeReader $ \case
  "accepted" -> Just Connection.Accepted
  "blocked" -> Just Connection.Blocked
  "pending" -> Just Connection.Pending
  "ignored" -> Just Connection.Ignored
  "sent" -> Just Connection.Sent
  "cancelled" -> Just Connection.Cancelled
  _ -> Nothing

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option (maybeReader URI.parseURI)

readOptions :: Handlers m -> IO (RunConfig m)
readOptions h =
  execParser $
    info
      (runConfigParser h <**> helper)
      (fullDesc <> progDesc "CLI for Wire")
