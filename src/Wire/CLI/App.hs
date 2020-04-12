module Wire.CLI.App where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import Polysemy
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend.HTTP as HTTPBackend
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store.File as FileStore

runApp :: Sem '[Store, Backend, Embed IO] () -> IO ()
runApp app = HTTP.withOpenSSL $ do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextLoadSystemCerts ctx
  mgr <- HTTP.newManager $ HTTP.opensslManagerSettings $ pure ctx
  runM . HTTPBackend.run "wire-cli-label" mgr . FileStore.run "/tmp" $ app
