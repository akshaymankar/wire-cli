module Wire.CLI.Error where

import qualified System.CryptoBox as CBox
import qualified Network.HTTP.Client as HTTP
import Control.Exception (Exception)

data WireCLIError
  = UnexpectedCryptoBoxError (CBox.Result ())
  | NotLoggedIn
  | NoConversationsFound
  | ErrorInvalidState InvalidState
  | HttpException HTTP.HttpException
  | Http401
  deriving (Show)

data InvalidState = NoClientFound
  deriving (Show, Eq)

instance Exception WireCLIError
