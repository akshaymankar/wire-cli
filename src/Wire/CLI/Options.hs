{-# LANGUAGE OverloadedLabels #-}

module Wire.CLI.Options where

import Data.Domain
import Data.Handle
import Data.Id (ConvId, UserId)
import Data.Int
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Text
import qualified Data.Text as Text
import Lens.Family2
import Network.URI (URI)
import qualified Network.URI as URI
import Numeric.Natural (Natural)
import Options.Applicative
import qualified Proto.Messages as M
import Wire.API.Connection (Relation (..), UserConnection)
import Wire.API.Conversation (Conversation)
import Wire.API.User (Name (Name), SelfProfile, parseEmail)
import Wire.API.User.Identity (Email)
import Wire.API.User.Search
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
  ListConvs :: Command [Conversation]
  RegisterWireless :: RegisterWirelessOptions -> Command ()
  RequestActivationCode :: RequestActivationCodeOptions -> Command ()
  Register :: RegisterOptions -> Command ()
  SetHandle :: Handle -> Command ()
  Search :: SearchOptions -> Command (SearchResult Contact)
  SyncNotifications :: Command ()
  SyncConnections :: Command ()
  ListConnections :: ListConnsOptions -> Command [UserConnection]
  UpdateConnection :: UpdateConnOptions -> Command ()
  Connect :: Qualified UserId -> Command ()
  SendMessage :: SendMessageOptions -> Command ()
  ListMessages :: ListMessagesOptions -> Command [StoredMessage]
  SyncSelf :: Command ()
  GetSelf :: GetSelfOptions -> Command SelfProfile
  WatchNotifications :: Command ()

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
    searchOptionDomain :: Maybe Domain,
    searchOptionMax :: Range 1 500 Int32
  }

newtype ListConnsOptions = ListConnsOptions
  { status :: Maybe Relation
  }
  deriving (Eq, Show)

data UpdateConnOptions = UpdateConnOptions
  { updateConnTo :: Qualified UserId,
    updateConnStatus :: Relation
  }
  deriving (Eq, Show)

data SendMessageOptions = SendMessageOptions
  { sendMessageConv :: Qualified ConvId,
    sendMessageData :: M.GenericMessage'Content
  }

data ListMessagesOptions = ListMessagesOptions
  { listMessagesConv :: Qualified ConvId,
    listMessagesN :: Natural
  }

newtype GetSelfOptions = GetSelfOptions {getSelfForceRefresh :: Bool}

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
  subparser . mconcat $
    [ mkCmd "login" loginParser "login to wire",
      mkCmd "logout" logoutParser "logout of wire",
      mkCmd "sync-convs" (pure SyncConvs) "synchronise conversations with the server",
      mkCmd "list-convs" (pure ListConvs) "list conversations (doesn't fetch them from server)",
      mkCmd "sync-notifications" (pure SyncNotifications) "synchronise notifications with the server",
      mkCmd "register-wireless" registerWirelessParser "register as an anonymous user",
      mkCmd "register" registerParser "register an account with email",
      mkCmd "set-handle" setHandleParser "set handle for the account (also called 'username')",
      mkCmd "request-activation-code" requestActivationParser "request an activation code for registration",
      mkCmd "search" searchParser "search for a user",
      mkCmd "sync-connections" (pure SyncConnections) "synchronise connections with the server, only necessary if something went wrong with notifications",
      mkCmd "list-connections" listConnsParser "list connections",
      mkCmd "update-connection" updateConnParser "update connection",
      mkCmd "connect" connectParser "connect with a user",
      mkCmd "send-message" sendMessageParser "send message to a conversation",
      mkCmd "list-messages" listMessagesParser "list last N messages",
      mkCmd "refresh-token" (pure RefreshToken) "refresh access token",
      mkCmd "sync-self" (pure SyncSelf) "synchronize data about self from backend",
      mkCmd "get-self" getSelfParser "display information about self, will fetch only if not available in local storage. Use --force-refresh to refresh before showing.",
      mkCmd "watch-notifications" (pure WatchNotifications) "watch and process notfications"
    ]
  where
    mkCmd :: String -> Parser (Command a) -> String -> Mod CommandFields AnyCommand
    mkCmd c parser desc = command c (info (AnyCommand <$> parser <**> helper) (progDesc desc))

getSelfParser :: Parser (Command SelfProfile)
getSelfParser =
  GetSelf
    <$> ( GetSelfOptions
            <$> switch (long "force-refresh" <> help "refresh information from backend before showing it")
        )

listMessagesParser :: Parser (Command [StoredMessage])
listMessagesParser =
  ListMessages
    <$> ( ListMessagesOptions
            <$> ( Qualified
                    <$> option auto (long "conv" <> help "conversation id to list messages from")
                    <*> option readDomain (long "domain" <> help "domain of the conversation")
                )
            <*> option auto (long "number-of-messages" <> short 'n' <> help "number of messages to list (starting from the end)")
        )

sendMessageParser :: Parser (Command ())
sendMessageParser =
  SendMessage
    <$> ( SendMessageOptions
            <$> ( Qualified
                    <$> option auto (long "conv" <> help "conversation id to send message to")
                    <*> option readDomain (long "domain" <> help "domain of the conversation")
                )
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
    <$> ( Qualified
            <$> option auto (long "user-id" <> help "user id of the user to connect with")
            <*> option readDomain (long "domain" <> help "domain of the user")
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
emailParser = option (maybeReader (parseEmail . Text.pack)) (long "email" <> help "email address")

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

searchParser :: Parser (Command (SearchResult Contact))
searchParser =
  Search
    <$> ( SearchOptions
            <$> strOption (long "query" <> short 'q' <> help "search query")
            <*> optional (option readDomain (long "domain" <> short 'd' <> help "domain to search in"))
            <*> option auto (long "max" <> value (toRange (Proxy @10)) <> help "maximum number of events to return" <> showDefault)
        )

readDomain :: ReadM Domain
readDomain = eitherReader (mkDomain . Text.pack)

listConnsParser :: Parser (Command [UserConnection])
listConnsParser =
  ListConnections
    <$> ( ListConnsOptions
            <$> optional (option readConnRelation (long "status"))
        )

updateConnParser :: Parser (Command ())
updateConnParser =
  UpdateConnection
    <$> ( UpdateConnOptions
            <$> ( Qualified
                    <$> option auto (long "user-id" <> help "user id of the other user")
                    <*> option readDomain (long "domain" <> help "domain of the other user")
                )
            <*> option readConnRelation (long "status" <> help "one of: accepted, blocked, pending, ignored, sent, cancelled")
        )

readConnRelation :: ReadM Relation
readConnRelation = maybeReader $ \case
  "accepted" -> Just Accepted
  "blocked" -> Just Blocked
  "pending" -> Just Pending
  "ignored" -> Just Ignored
  "sent" -> Just Sent
  "cancelled" -> Just Cancelled
  _ -> Nothing

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option (maybeReader URI.parseURI)

readOptions :: IO RunConfig
readOptions =
  execParser $
    info
      (runConfigParser <**> helper)
      (fullDesc <> progDesc "CLI for Wire")
