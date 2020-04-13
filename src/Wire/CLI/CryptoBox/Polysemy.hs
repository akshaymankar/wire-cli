{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.CryptoBox.Polysemy where

import Data.Word
import Polysemy
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend.Prekey (Prekey)

data CryptoBox m a where
  RandomBytes :: Word32 -> CryptoBox m (CBox.Result [Word8])
  NewPrekey :: Word16 -> CryptoBox m (CBox.Result Prekey)

makeSem ''CryptoBox
