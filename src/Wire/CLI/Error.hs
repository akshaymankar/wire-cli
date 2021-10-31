module Wire.CLI.Error where

import qualified System.CryptoBox as CBox
import qualified Network.HTTP.Client as HTTP
import Control.Exception (Exception)
import Data.Text (Text)

data WireCLIError
  = UnexpectedCryptoBoxError (CBox.Result ())
  | NotLoggedIn
  | NoConversationsFound
  | ErrorInvalidState InvalidState
  | HttpException HTTP.HttpException
  | InvalidPrekey Text
  | Http401
  deriving (Show)

data InvalidState = NoClientFound | NoSelfUserSaved
  deriving (Show, Eq)

instance Exception WireCLIError
