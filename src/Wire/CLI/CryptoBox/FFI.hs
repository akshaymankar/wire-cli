module Wire.CLI.CryptoBox.FFI where

import qualified Data.ByteString as BS
import Data.Word
import Polysemy
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend.Prekey (Prekey (Prekey))
import Wire.CLI.CryptoBox.Polysemy

run :: Member (Embed IO) r => CBox.Box -> Sem (CryptoBox ': r) a -> Sem r a
run box = interpret $
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
  pure $ Prekey i <$> prekey

-- | Can we just throw?
sequenceResult :: Monad m => CBox.Result (m a) -> m (CBox.Result a)
sequenceResult = \case
  CBox.Success ma -> CBox.Success <$> ma
  CBox.StorageError -> pure CBox.StorageError
  CBox.NoSession -> pure CBox.NoSession
  CBox.DecodeError -> pure CBox.DecodeError
  CBox.RemoteIdentityChanged -> pure CBox.RemoteIdentityChanged
  CBox.InvalidSignature -> pure CBox.InvalidSignature
  CBox.InvalidMessage -> pure CBox.InvalidMessage
  CBox.DuplicateMessage -> pure CBox.DuplicateMessage
  CBox.TooDistantFuture -> pure CBox.TooDistantFuture
  CBox.OutdatedMessage -> pure CBox.OutdatedMessage
  CBox.Utf8Error -> pure CBox.Utf8Error
  CBox.NulError -> pure CBox.NulError
  CBox.EncodeError -> pure CBox.EncodeError
  CBox.IdentityError -> pure CBox.IdentityError
  CBox.NoPrekey -> pure CBox.NoPrekey
  CBox.Panic -> pure CBox.Panic
  CBox.Unknown n -> pure $ CBox.Unknown n
