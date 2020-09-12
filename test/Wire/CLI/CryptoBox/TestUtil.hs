module Wire.CLI.CryptoBox.TestUtil where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import Polysemy
import qualified System.CryptoBox as CBox
import Test.Hspec (expectationFailure)
import Test.QuickCheck (Gen, elements)
import Wire.CLI.Backend.Prekey (Prekey)
import qualified Wire.CLI.CryptoBox as CryptoBox
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import qualified Wire.CLI.App as App
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

getTempCBox :: Member (Embed IO) r => Sem r CBox.Box
getTempCBox = embed $ do
   tmpDir <- getCanonicalTemporaryDirectory
   cboxDir <- createTempDirectory tmpDir "temp-crypt"
   putStrLn cboxDir
   App.openCBox cboxDir

assertSuccess :: (MonadIO m, Show a, HasCallStack) => CBox.Result a -> m a
assertSuccess (CBox.Success x) = pure x
assertSuccess x = liftIO $ expectationFailure ("expected Success, got: " <> show x) >> error "Impossible!"

decryptWithBox :: (Member (Embed IO) r, HasCallStack) => CBox.Box -> CBox.SID -> ByteString -> Sem r (CBox.Session, ByteString)
decryptWithBox box sid cipher = assertSuccess =<< CryptoBoxFFI.run box (CryptoBox.sessionFromMessage sid cipher)

newPrekeyWithBox :: Member (Embed IO) r => CBox.Box -> Word16 -> Sem r Prekey
newPrekeyWithBox box n = assertSuccess =<< (CryptoBoxFFI.run box $ CryptoBox.newPrekey n)

sessionWithBox :: Member (Embed IO) r => CBox.Box -> CBox.SID -> Prekey -> Sem r CBox.Session
sessionWithBox box sid pk = assertSuccess =<< (CryptoBoxFFI.run box $ CryptoBox.sessionFromPrekey sid pk)

anyFailureExcept :: [CBox.Result ()] -> Gen (CBox.Result ())
anyFailureExcept excepted =
  let allErrors =
        Set.fromList
          [ CBox.StorageError,
            CBox.NoSession,
            CBox.DecodeError,
            CBox.RemoteIdentityChanged,
            CBox.InvalidSignature,
            CBox.InvalidMessage,
            CBox.DuplicateMessage,
            CBox.TooDistantFuture,
            CBox.OutdatedMessage,
            CBox.Utf8Error,
            CBox.NulError,
            CBox.EncodeError,
            CBox.IdentityError,
            CBox.NoPrekey,
            CBox.Panic,
            CBox.Unknown 1234
          ]
   in elements $ Set.toList $ Set.difference allErrors (Set.fromList excepted)

castCBoxError :: CBox.Result a -> CBox.Result b
castCBoxError = \case
  CBox.Success _ -> error "castCBoxError can only type cast errors"
  CBox.StorageError -> CBox.StorageError
  CBox.NoSession -> CBox.NoSession
  CBox.DecodeError -> CBox.DecodeError
  CBox.RemoteIdentityChanged -> CBox.RemoteIdentityChanged
  CBox.InvalidSignature -> CBox.InvalidSignature
  CBox.InvalidMessage -> CBox.InvalidMessage
  CBox.DuplicateMessage -> CBox.DuplicateMessage
  CBox.TooDistantFuture -> CBox.TooDistantFuture
  CBox.OutdatedMessage -> CBox.OutdatedMessage
  CBox.Utf8Error -> CBox.Utf8Error
  CBox.NulError -> CBox.NulError
  CBox.EncodeError -> CBox.EncodeError
  CBox.IdentityError -> CBox.IdentityError
  CBox.NoPrekey -> CBox.NoPrekey
  CBox.Panic -> CBox.Panic
  CBox.Unknown x -> CBox.Unknown x
