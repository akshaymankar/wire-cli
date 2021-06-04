module Wire.CLI.Error where

import qualified System.CryptoBox as CBox

data WireCLIError
  = UnexpectedCryptoBoxError (CBox.Result ())
  | NotLoggedIn
  | NoConversationsFound
  | ErrorInvalidState InvalidState
  deriving (Show, Eq)

data InvalidState = NoClientFound
  deriving (Show, Eq)
