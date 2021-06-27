module Wire.CLI.Error where

import qualified System.CryptoBox as CBox
import qualified Network.HTTP.Client as HTTP

data WireCLIError
  = UnexpectedCryptoBoxError (CBox.Result ())
  | NotLoggedIn
  | NoConversationsFound
  | ErrorInvalidState InvalidState
  | HttpException HTTP.HttpException
  deriving (Show)

data InvalidState = NoClientFound
  deriving (Show, Eq)
