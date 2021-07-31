{-# LANGUAGE OverloadedLabels #-}

module Wire.CLI.Options where

import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import Data.Text
import Lens.Family2
import Network.URI (URI)
import qualified Network.URI as URI
import Numeric.Natural (Natural)
import Options.Applicative
import qualified Proto.Messages as M
import Wire.CLI.Backend.CommonTypes (Name (..))
import Wire.CLI.Backend.Connection (Connection, ConnectionMessage (..), ConnectionRequest (..))
import qualified Wire.CLI.Backend.Connection as Connection
import Wire.CLI.Backend.Conv (Conv, ConvId (..))
import Wire.CLI.Backend.Search (SearchResults)
import Wire.CLI.Backend.User (Email (..), Handle (..), UserId (..))
import Wire.CLI.Store (StoredMessage)

newtype StoreConfig = StoreConfig {baseDir :: FilePath}

newtype Config = Config {storeConfig :: StoreConfig}

data RunConfig = RunConfig {config :: Config, cmd :: AnyCommand}

data Command a where
  -- | 'Nothing' means login successful, 'Just' contains the error.
  Login :: LoginOptions -> Command (Maybe Text)
  Logout :: Command ()
  RefreshToken :: Command ()
  SyncConvs :: Command ()
  ListConvs :: Command [Conv]
  RegisterWireless :: RegisterWirelessOptions -> Command ()
  RequestActivationCode :: RequestActivationCodeOptions -> Command ()
  Register :: RegisterOptions -> Command ()
  SetHandle :: Handle -> Command ()
  Search :: SearchOptions -> Command SearchResults
  SyncNotifications :: Command ()
  SyncConnections :: Command ()
  ListConnections :: ListConnsOptions -> Command [Connection]
  UpdateConnection :: UpdateConnOptions -> Command ()
  Connect :: ConnectionRequest -> Command ()
  SendMessage :: SendMessageOptions -> Command ()
  ListMessages :: ListMessagesOptions -> Command [StoredMessage]

data AnyCommand where
  AnyCommand :: Command a -> AnyCommand

data LoginIdentity
  = LoginHandle Text
  | LoginEmail Text
  deriving (Eq, Show)

data LoginOptions = LoginOptions
  { loginServer :: URI,
    loginIdentity :: LoginIdentity,
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

data SendMessageOptions = SendMessageOptions
  { sendMessageConv :: ConvId,
    sendMessageData :: M.GenericMessage'Content
  }

data ListMessagesOptions = ListMessagesOptions
  { listMessagesConv :: ConvId,
    listMessagesN :: Natural
  }

runConfigParser :: Parser RunConfig
runConfigParser = RunConfig <$> fmap Config parseStoreConfig <*> commandParser

parseStoreConfig :: Parser StoreConfig
parseStoreConfig =
  StoreConfig
    <$> strOption
      ( long "file-store-path"
          <> value "/tmp"
          <> help "base directory for client state"
          <> showDefault
      )

commandParser :: Parser AnyCommand
commandParser =
  subparser $
    command "login" (info (AnyCommand <$> loginParser <**> helper) (progDesc "login to wire"))
      <> command "logout" (info (AnyCommand <$> logoutParser <**> helper) (progDesc "logout of wire"))
      <> command "sync-convs" (info (pure (AnyCommand SyncConvs) <**> helper) (progDesc "synchronise conversations with the server"))
      <> command "list-convs" (info (pure (AnyCommand ListConvs) <**> helper) (progDesc "list conversations (doesn't fetch them from server)"))
      <> command "sync-notifications" (info (pure (AnyCommand SyncNotifications) <**> helper) (progDesc "synchronise notifications with the server"))
      <> command "register-wireless" (info (AnyCommand <$> registerWirelessParser <**> helper) (progDesc "register as an anonymous user"))
      <> command "register" (info (AnyCommand <$> registerParser <**> helper) (progDesc "register an account with email"))
      <> command "set-handle" (info (AnyCommand <$> setHandleParser <**> helper) (progDesc "set handle for the account (also called 'username')"))
      <> command "request-activation-code" (info (AnyCommand <$> requestActivationParser <**> helper) (progDesc "request an activation code for registration"))
      <> command "search" (info (AnyCommand <$> searchParser <**> helper) (progDesc "search for a user"))
      <> command "sync-connections" (info (pure (AnyCommand SyncConnections) <**> helper) (progDesc "synchronise connections with the server, only necessary if something went wrong with notifications"))
      <> command "list-connections" (info (AnyCommand <$> listConnsParser <**> helper) (progDesc "list connections"))
      <> command "update-connection" (info (AnyCommand <$> updateConnParser <**> helper) (progDesc "update connection"))
      <> command "connect" (info (AnyCommand <$> connectParser <**> helper) (progDesc "connect with a user"))
      <> command "send-message" (info (AnyCommand <$> sendMessageParser <**> helper) (progDesc "send message to a conversation"))
      <> command "list-messages" (info (AnyCommand <$> listMessagesParser <**> helper) (progDesc "list last N messages"))
      <> command "refresh-token" (info (pure (AnyCommand RefreshToken) <**> helper) (progDesc "refresh access token"))

listMessagesParser :: Parser (Command [StoredMessage])
listMessagesParser =
  ListMessages
    <$> ( ListMessagesOptions
            <$> (ConvId <$> strOption (long "conv" <> help "conversation id to list messages from"))
            <*> option auto (long "number-of-messages" <> short 'n' <> help "number of messages to list (starting from the end)")
        )

sendMessageParser :: Parser (Command ())
sendMessageParser =
  SendMessage
    <$> ( SendMessageOptions
            <$> (ConvId <$> strOption (long "to" <> help "conversation id to send message to"))
            <*> (mkTextMessage <$> strOption (long "message" <> short 'm' <> help "message to be sent"))
        )
  where
    mkTextMessage :: Text -> M.GenericMessage'Content
    mkTextMessage t =
      M.GenericMessage'Text (Proto.defMessage & #content .~ t)

setHandleParser :: Parser (Command ())
setHandleParser =
  SetHandle
    <$> (Handle <$> strOption (long "handle" <> help "handle for the account"))

connectParser :: Parser (Command ())
connectParser =
  Connect
    <$> ( ConnectionRequest
            <$> (UserId <$> strOption (long "user-id" <> help "user id of the user to connect with"))
            <*> strOption (long "conv-name" <> help "name of the conversation")
            <*> (ConnectionMessage <$> strOption (long "message" <> help "connection message"))
        )

requestActivationParser :: Parser (Command ())
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

registerParser :: Parser (Command ())
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

loginParser :: Parser (Command (Maybe Text))
loginParser =
  Login
    <$> ( LoginOptions
            <$> serverParser
            <*> ( LoginHandle <$> strOption (long "username" <> help "username to login as")
                    <|> LoginEmail <$> strOption (long "email" <> help "email to login as")
                )
            <*> strOption (long "password" <> help "password for the user")
        )

serverParser :: Parser URI
serverParser = uriOption (long "server" <> help "server address")

logoutParser :: Parser (Command ())
logoutParser = pure Logout

registerWirelessParser :: Parser (Command ())
registerWirelessParser =
  RegisterWireless
    <$> ( RegisterWirelessOptions
            <$> serverParser
            <*> nameParser
        )

nameParser :: Parser Name
nameParser = Name <$> strOption (long "name" <> help "name of user, doesn't have to be unique")

searchParser :: Parser (Command SearchResults)
searchParser =
  Search
    <$> ( SearchOptions
            <$> strOption (long "query" <> short 'q' <> help "search query")
            <*> option auto (long "max" <> value 10 <> help "maximum number of events to return" <> showDefault)
        )

listConnsParser :: Parser (Command [Connection])
listConnsParser =
  ListConnections
    <$> ( ListConnsOptions
            <$> optional (option readConnRelation (long "status"))
        )

updateConnParser :: Parser (Command ())
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

readOptions :: IO RunConfig
readOptions =
  execParser $
    info
      (runConfigParser <**> helper)
      (fullDesc <> progDesc "CLI for Wire")
