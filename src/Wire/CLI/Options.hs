module Wire.CLI.Options where

import Data.Text
import Network.URI (URI)
import qualified Network.URI as URI
import Options.Applicative
import Wire.CLI.Backend.CommonTypes (Name (..))
import Wire.CLI.Backend.Conv (Conv)

data Command m
  = Login LoginOptions
  | Logout
  | SyncConvs
  | ListConvs ([Conv] -> m ())
  | RegisterWireless RegisterWirelessOptions
  | SyncNotifications

newtype Handlers m = Handlers {listConvHandler :: [Conv] -> m ()}

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

parseCommand :: Handlers m -> Parser (Command m)
parseCommand h =
  subparser $
    command "login" (info loginParser (progDesc "login to wire"))
      <> command "logout" (info logoutParser (progDesc "logout of wire"))
      <> command "sync-convs" (info (pure SyncConvs) (progDesc "synchronise conversations with the server"))
      <> command "list-convs" (info (pure $ ListConvs $ listConvHandler h) (progDesc "list conversations (doesn't fetch them from server)"))
      <> command "sync-notifications" (info (pure SyncNotifications) (progDesc "synchronise notifications with the server"))
      <> command "register-wireless" (info registerWirelessParser (progDesc "register as anonymous user"))

loginParser :: Parser (Command m)
loginParser =
  Login
    <$> ( LoginOptions
            <$> uriOption (long "server" <> help "server address")
            <*> strOption (long "username" <> help "username to login as")
            <*> strOption (long "password" <> help "password for the user")
        )

logoutParser :: Parser (Command m)
logoutParser = pure Logout

registerWirelessParser :: Parser (Command m)
registerWirelessParser =
  RegisterWireless
    <$> ( RegisterWirelessOptions
            <$> uriOption (long "server" <> help "server address")
            <*> (Name <$> strOption (long "name" <> help "name of user, doesn't have to be unique"))
        )

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option (maybeReader URI.parseURI)

readOptions :: Handlers m -> IO (Command m)
readOptions h =
  execParser $
    info
      (parseCommand h <**> helper)
      (fullDesc <> progDesc "CLI for Wire")
