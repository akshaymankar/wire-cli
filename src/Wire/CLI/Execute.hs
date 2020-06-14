module Wire.CLI.Execute where

import Control.Monad ((<=<), replicateM)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI (URI)
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Connection as Connection
import qualified Wire.CLI.Conv as Conv
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Notification as Notification
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

execute :: Members '[Backend, Store, CryptoBox, Random, Error WireCLIError] r => Opts.Command (Sem r) -> Sem r ()
execute = \case
  Opts.Login loginOpts -> performLogin loginOpts
  Opts.Logout -> error "Not implemented"
  Opts.SyncConvs -> Conv.sync
  Opts.ListConvs f -> f =<< Conv.list
  Opts.SyncNotifications -> Notification.sync
  Opts.RegisterWireless opts -> performWirelessRegister opts
  Opts.Search opts f -> f =<< search opts
  Opts.RequestActivationCode opts -> Backend.requestActivationCode opts
  Opts.Register opts -> Backend.register opts >>= getTokenAndRegisterClient (Opts.registerServer opts)
  Opts.SyncConnections -> Connection.sync
  Opts.ListConnections f -> f =<< Store.getConnections
  Opts.Connect cr -> connect cr

performLogin :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Opts.LoginOptions -> Sem r ()
performLogin opts = do
  res <- Backend.login opts
  case res of
    Backend.LoginFailure e -> Error.throw $ WireCLIError.LoginFailed e
    Backend.LoginSuccess t -> do
      let serverCred = Backend.ServerCredential (Opts.loginServer opts) t
      Store.saveCreds serverCred
      preKeys <- mapM (throwCBoxError <=< CryptoBox.newPrekey) [0 .. 99]
      lastKey <- throwCBoxError =<< CryptoBox.newPrekey maxBound
      let newClient = Backend.NewClient "wire-cli-cookie-label" lastKey (Opts.loginPassword opts) "wire-cli" Backend.Permanent preKeys Backend.Desktop "wire-cli"
      client <- Backend.registerClient serverCred newClient
      Store.saveClientId (Backend.clientId client)

throwCBoxError :: (Member (Error WireCLIError) r) => CBox.Result a -> Sem r a
throwCBoxError res =
  case CryptoBox.resultToEither res of
    Left e -> Error.throw $ WireCLIError.UnexpectedCryptoBoxError e
    Right a -> pure a

performWirelessRegister :: Members '[Backend, Store, CryptoBox, Error WireCLIError, Random] r => Opts.RegisterWirelessOptions -> Sem r ()
performWirelessRegister opts = do
  cookies <- Backend.registerWireless opts
  getTokenAndRegisterClient (Opts.registerWirelessServer opts) cookies

getTokenAndRegisterClient :: Members '[Backend, Store, CryptoBox, Error WireCLIError, Random] r => URI -> [Backend.WireCookie] -> Sem r ()
getTokenAndRegisterClient server cookies = do
  token <- Backend.refreshToken server cookies
  let serverCred = Backend.ServerCredential server (Backend.Credential cookies token)
  Store.saveCreds serverCred
  -- Untested random password generation
  password <- Text.pack <$> replicateM 15 (Random.randomR ('a', 'z'))
  registerClient serverCred password

registerClient :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Backend.ServerCredential -> Text -> Sem r ()
registerClient serverCred password = do
  preKeys <- mapM (throwCBoxError <=< CryptoBox.newPrekey) [0 .. 99]
  lastKey <- throwCBoxError =<< CryptoBox.newPrekey maxBound
  let newClient = Backend.NewClient "wire-cli-cookie-label" lastKey password "wire-cli" Backend.Permanent preKeys Backend.Desktop "wire-cli"
  client <- Backend.registerClient serverCred newClient
  Store.saveClientId (Backend.clientId client)

search :: Members '[Backend, Store, Error WireCLIError] r => Opts.SearchOptions -> Sem r Backend.SearchResults
search opts = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  Backend.search serverCreds opts

connect :: Members '[Backend, Store, Error WireCLIError] r => Backend.ConnectionRequest -> Sem r ()
connect cr = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  Backend.connect serverCreds cr
