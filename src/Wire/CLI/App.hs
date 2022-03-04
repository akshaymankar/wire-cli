module Wire.CLI.App where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import Polysemy (Embed, Member, Sem, embed, runM)
import Polysemy.Async (Async)
import qualified Polysemy.Async as Async
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified System.CryptoBox as CBox
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend.HTTP as HTTPBackend
import Wire.CLI.Chan (ReadChan, WriteChan, NewChan)
import qualified Wire.CLI.Chan as Chan
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.Display (Display)
import qualified Wire.CLI.Display.Print as PrintDisplay
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store.File as FileStore
import Wire.CLI.UUIDGen (UUIDGen)
import qualified Wire.CLI.UUIDGen as UUIDGen

runApp :: Opts.Config -> Sem '[CryptoBox, Store, Backend, Display, Random, UUIDGen, NewChan, ReadChan, WriteChan, Error WireCLIError, Async, Embed IO] () -> IO ()
runApp cfg app = HTTP.withOpenSSL $ do
  mgr <- HTTP.newManager $ HTTP.opensslManagerSettings sslContext
  cbox <- openCBox . cboxDir . Opts.baseDir . Opts.storeConfig $ cfg
  runM
    . Async.asyncToIO
    . failOnError
    . Chan.runWrite
    . Chan.runRead
    . Chan.runNew
    . UUIDGen.run
    . Random.runRandomIO
    . PrintDisplay.run
    . HTTPBackend.run "wire-cli-label" mgr
    . FileStore.run (Opts.baseDir $ Opts.storeConfig cfg)
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

openCBox :: FilePath -> IO CBox.Box
openCBox dir = do
  Dir.createDirectoryIfMissing True dir
  CBox.open dir >>= \case
    CBox.Success b -> pure b
    err -> error $ "Failed to open crypto box: " ++ show err

cboxDir :: FilePath -> FilePath
cboxDir = (</> "cryptobox")
