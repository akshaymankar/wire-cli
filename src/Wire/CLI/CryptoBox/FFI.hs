module Wire.CLI.CryptoBox.FFI where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Tuple.Extra (secondM)
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
      GetSession sid -> CBox.session box sid
      SessionFromPrekey sid (Prekey _ (Base64ByteString bs)) -> CBox.sessionFromPrekey box sid bs
      SessionFromMessage sid cipher -> mkSessionFromMessage box sid cipher
      Encrypt ses msg -> encryptMessage ses msg
      Decrypt ses msg -> decryptMessage ses msg
      Save ses -> either (error . show) id . resultToEither <$> CBox.save box ses

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

encryptMessage :: CBox.Session -> ByteString -> IO (CBox.Result ByteString)
encryptMessage ses plain = do
  res <- CBox.encrypt ses plain
  sequenceResult $ CBox.copyBytes <$> res

decryptMessage :: CBox.Session -> ByteString -> IO (CBox.Result ByteString)
decryptMessage ses cipher = do
  res <- CBox.decrypt ses cipher
  sequenceResult $ CBox.copyBytes <$> res

mkSessionFromMessage :: CBox.Box -> CBox.SID -> ByteString -> IO (CBox.Result (CBox.Session, ByteString))
mkSessionFromMessage box sid cipher = do
  res <- CBox.sessionFromMessage box sid cipher
  sequenceResult $ (secondM CBox.copyBytes) <$> res
