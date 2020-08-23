module Wire.CLI.CryptoBox.FFI where

import qualified Data.ByteString as BS
import Data.Word
import Polysemy
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend.Prekey (Prekey (Prekey))
import Wire.CLI.CryptoBox.Effect
import Wire.CLI.CryptoBox.Util
import Wire.CLI.Util.ByteStringJSON (Base64ByteString (..))

run :: Member (Embed IO) r => CBox.Box -> Sem (CryptoBox ': r) a -> Sem r a
run box =
  interpret $
    embed . \case
      RandomBytes n -> getRandomBytes box n
      NewPrekey i -> genPrekey box i

getRandomBytes :: CBox.Box -> Word32 -> IO (CBox.Result [Word8])
getRandomBytes box n = do
  res <- CBox.randomBytes box n
  resBS <- sequenceResult $ CBox.copyBytes <$> res
  pure $ BS.unpack <$> resBS

genPrekey :: CBox.Box -> Word16 -> IO (CBox.Result Prekey)
genPrekey box i = do
  res <- CBox.newPrekey box i
  prekey <- sequenceResult $ CBox.copyBytes . CBox.prekey <$> res
  pure $ Prekey i . Base64ByteString <$> prekey
