module Wire.CLI.App where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified System.CryptoBox as CBox
import qualified System.Directory as Dir
import System.FilePath
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend.HTTP as HTTPBackend
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.Display (Display)
import Wire.CLI.Display.Print as PrintDisplay
import Wire.CLI.Error (WireCLIError)
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store.File as FileStore

runApp :: Sem '[CryptoBox, Store, Backend, Display, Random, Error WireCLIError, Embed IO] () -> IO ()
runApp app = HTTP.withOpenSSL $ do
  mgr <- HTTP.newManager $ HTTP.opensslManagerSettings sslContext
  cbox <- openCBox
  runM
    . failOnError
    . Random.runRandomIO
    . PrintDisplay.run
    . HTTPBackend.run "wire-cli-label" mgr
    . FileStore.run "/tmp"
    . CryptoBoxFFI.run cbox
    $ app

failOnError :: Member (Embed IO) r => Sem (Error WireCLIError ': r) a -> Sem r a
failOnError s =
  Error.runError s >>= \case
    Right a -> pure a
    Left e -> embed $ fail $ "Error occurred: " <> show e

sslContext :: IO SSL.SSLContext
sslContext = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextLoadSystemCerts ctx
  pure ctx

openCBox :: IO CBox.Box
openCBox = do
  tmpDir <- Dir.getTemporaryDirectory
  let cboxDir = tmpDir </> "cryptobox"
  Dir.createDirectoryIfMissing True cboxDir
  CBox.open cboxDir >>= \case
    CBox.Success b -> pure b
    err -> error $ "Failed to open crypto box: " ++ show err
