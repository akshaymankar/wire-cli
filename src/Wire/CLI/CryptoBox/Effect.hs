module Wire.CLI.CryptoBox.Effect where

import Control.Algebra
import Data.Kind (Type)
import Data.Word
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend.Prekey (Prekey)

data CryptoBox (m :: Type -> Type) k where
  RandomBytes :: Word32 -> CryptoBox m (CBox.Result [Word8])
  NewPrekey :: Word16 -> CryptoBox m (CBox.Result Prekey)

randomBytes :: Has CryptoBox sig m => Word32 -> m (CBox.Result [Word8])
randomBytes = send . RandomBytes

newPrekey :: Has CryptoBox sig m => Word16 -> m (CBox.Result Prekey)
newPrekey = send . NewPrekey
