module Wire.CLI.App where

import Control.Carrier.Lift (LiftC, runM)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import qualified System.CryptoBox as CBox
import qualified System.Directory as Dir
import System.FilePath
import qualified Wire.CLI.Backend.HTTP as HTTPBackend
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import qualified Wire.CLI.Store.File as FileStore

runApp :: CryptoBoxFFI.CryptoBoxFFI (FileStore.File (HTTPBackend.HTTP (LiftC IO))) a -> IO a
runApp app = HTTP.withOpenSSL $ do
  mgr <- HTTP.newManager $ HTTP.opensslManagerSettings sslContext
  cbox <- openCBox
  runM
    . HTTPBackend.run "wire-cli-label" mgr
    . FileStore.run "/tmp"
    . CryptoBoxFFI.run cbox
    $ app

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
