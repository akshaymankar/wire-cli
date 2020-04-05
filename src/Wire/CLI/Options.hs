module Wire.CLI.Options where

import Data.Text
import Options.Applicative

data Command
  = Login LoginOptions
  | Logout
  deriving (Eq, Show)

data LoginOptions
  = LoginOptions
      { loginServer :: Text,
        loginHandle :: Text,
        loginPassword :: Text
      }
  deriving (Eq, Show)

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "login" (info loginParser (progDesc "login to wire"))
      <> command "logout" (info logoutParser (progDesc "logout of wire"))

loginParser :: Parser Command
loginParser =
  Login
    <$> ( LoginOptions
            <$> strOption (long "server" <> help "server address")
            <*> strOption (long "username" <> help "username to login as")
            <*> strOption (long "password" <> help "password for the user")
        )

logoutParser :: Parser Command
logoutParser = pure Logout

readOptions :: IO Command
readOptions =
  execParser $
    info
      (parseCommand <**> helper)
      (fullDesc <> progDesc "CLI for Wire")
