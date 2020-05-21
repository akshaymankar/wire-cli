module Wire.CLI.Error where

import Data.Text (Text)
import qualified System.CryptoBox as CBox

data WireCLIError
  = UnexpectedCryptoBoxError (CBox.Result ())
  | LoginFailed Text
  | NotLoggedIn
  | NoConversationsFound
  | ErrorInvalidState InvalidState
  deriving (Show, Eq)

data InvalidState = NoClientFound
  deriving (Show, Eq)
