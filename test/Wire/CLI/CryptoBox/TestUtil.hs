module Wire.CLI.CryptoBox.TestUtil where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import Polysemy (Embed, Member, Sem, embed)
import qualified System.CryptoBox as CBox
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Hspec (expectationFailure)
import Test.QuickCheck
import Wire.API.User.Client.Prekey (Prekey (prekeyKey))
import qualified Wire.CLI.App as App
import qualified Wire.CLI.CryptoBox as CryptoBox
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI

getTempCBoxDir :: Member (Embed IO) r => Sem r FilePath
getTempCBoxDir = embed $ do
  tmpDir <- getCanonicalTemporaryDirectory
  createTempDirectory tmpDir "temp-crypt"

getTempCBox :: Member (Embed IO) r => Sem r CBox.Box
getTempCBox = do
  cboxDir <- getTempCBoxDir
  embed $ App.openCBox cboxDir

assertSuccess :: (MonadIO m, Show a, HasCallStack) => CBox.Result a -> m a
assertSuccess (CBox.Success x) = pure x
assertSuccess x = liftIO $ expectationFailure ("expected Success, got: " <> show x) >> error "Impossible!"

decryptWithBox :: (Member (Embed IO) r, HasCallStack) => CBox.Box -> CBox.SID -> ByteString -> Sem r (CBox.Session, ByteString)
decryptWithBox box sid cipher = assertSuccess =<< CryptoBoxFFI.run box (CryptoBox.sessionFromMessage sid cipher)

newPrekeyWithBox :: Member (Embed IO) r => CBox.Box -> Word16 -> Sem r Prekey
newPrekeyWithBox box n = assertSuccess =<< CryptoBoxFFI.run box (CryptoBox.newPrekey n)

generateArbitraryPrekey :: Member (Embed IO) r => Sem r Prekey
generateArbitraryPrekey = do
  box <- getTempCBox
  n <- embed $ generate arbitrary
  newPrekeyWithBox box n

sessionWithBox :: Member (Embed IO) r => CBox.Box -> CBox.SID -> Prekey -> Sem r CBox.Session
sessionWithBox box sid pk = do
  let pkBS = decodeBase64Lenient (Text.encodeUtf8 (prekeyKey pk))
  assertSuccess =<< CryptoBoxFFI.run box (CryptoBox.sessionFromPrekey sid pkBS)

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
