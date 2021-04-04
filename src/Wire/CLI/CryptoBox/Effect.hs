{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.CryptoBox.Effect where

import Data.ByteString (ByteString)
import Data.Word
import Polysemy
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend.Prekey (Prekey)

data CryptoBox m a where
  RandomBytes :: Word32 -> CryptoBox m (CBox.Result [Word8])
  NewPrekey :: Word16 -> CryptoBox m (CBox.Result Prekey)
  GetSession :: CBox.SID -> CryptoBox m (CBox.Result CBox.Session)
  SessionFromPrekey :: CBox.SID -> Prekey -> CryptoBox m (CBox.Result CBox.Session)
  SessionFromMessage :: CBox.SID -> ByteString -> CryptoBox m (CBox.Result (CBox.Session, ByteString))
  Encrypt :: CBox.Session -> ByteString -> CryptoBox m (CBox.Result ByteString)
  Decrypt :: CBox.Session -> ByteString -> CryptoBox m (CBox.Result ByteString)
  Save :: CBox.Session -> CryptoBox m (CBox.Result ())

makeSem ''CryptoBox
