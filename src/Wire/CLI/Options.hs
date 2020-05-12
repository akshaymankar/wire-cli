module Wire.CLI.Options where

import Data.Text
import Network.URI (URI)
import qualified Network.URI as URI
import Options.Applicative
import Wire.CLI.Backend.Conv (Conv)

data Command m
  = Login LoginOptions
  | Logout
  | SyncConvs
  | ListConvs ([Conv] -> m ())

newtype Handlers m = Handlers {listConvHandler :: [Conv] -> m ()}

data LoginOptions = LoginOptions
  { loginServer :: URI,
    loginHandle :: Text,
    loginPassword :: Text
  }
  deriving (Eq, Show)

parseCommand :: Handlers m -> Parser (Command m)
parseCommand h =
  subparser $
    command "login" (info loginParser (progDesc "login to wire"))
      <> command "logout" (info logoutParser (progDesc "logout of wire"))
      <> command "sync-convs" (info (pure SyncConvs) (progDesc "synchronise conversations with the server"))
      <> command "list-convs" (info (pure $ ListConvs $ listConvHandler h) (progDesc "list conversations (doesn't fetch them from server)"))

loginParser :: Parser (Command m)
loginParser =
  Login
    <$> ( LoginOptions
            <$> uriOption (long "server" <> help "server address")
            <*> strOption (long "username" <> help "username to login as")
            <*> strOption (long "password" <> help "password for the user")
        )
  where
    uriOption = option (maybeReader URI.parseURI)

logoutParser :: Parser (Command m)
logoutParser = pure Logout

readOptions :: Handlers m -> IO (Command m)
readOptions h =
  execParser $
    info
      (parseCommand h <**> helper)
      (fullDesc <> progDesc "CLI for Wire")
