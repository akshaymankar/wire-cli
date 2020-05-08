module Wire.CLI.Error where

import qualified System.CryptoBox as CBox

newtype WireCLIError = UnexpectedCryptoBoxError (CBox.Result ())
  deriving (Show)
