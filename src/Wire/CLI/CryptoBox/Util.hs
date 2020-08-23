module Wire.CLI.CryptoBox.Util where

import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import qualified System.CryptoBox as CBox
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError

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

resultToEither :: CBox.Result a -> Either (CBox.Result ()) a
resultToEither = \case
  CBox.Success a -> Right a
  CBox.StorageError -> Left CBox.StorageError
  CBox.NoSession -> Left CBox.NoSession
  CBox.DecodeError -> Left CBox.DecodeError
  CBox.RemoteIdentityChanged -> Left CBox.RemoteIdentityChanged
  CBox.InvalidSignature -> Left CBox.InvalidSignature
  CBox.InvalidMessage -> Left CBox.InvalidMessage
  CBox.DuplicateMessage -> Left CBox.DuplicateMessage
  CBox.TooDistantFuture -> Left CBox.TooDistantFuture
  CBox.OutdatedMessage -> Left CBox.OutdatedMessage
  CBox.Utf8Error -> Left CBox.Utf8Error
  CBox.NulError -> Left CBox.NulError
  CBox.EncodeError -> Left CBox.EncodeError
  CBox.IdentityError -> Left CBox.IdentityError
  CBox.NoPrekey -> Left CBox.NoPrekey
  CBox.Panic -> Left CBox.Panic
  CBox.Unknown x -> Left $ CBox.Unknown x

resultToError :: Member (Error WireCLIError) r => CBox.Result a -> Sem r a
resultToError res = case resultToEither res of
  Right x -> pure x
  Left err -> Error.throw $ WireCLIError.UnexpectedCryptoBoxError err
