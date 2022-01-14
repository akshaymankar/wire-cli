module Wire.CLI.Execute where

import Control.Monad (replicateM, void, when, (<=<))
import Data.Handle (Handle)
import Data.Id
import Data.Maybe (isNothing)
import Data.Misc (PlainTextPassword (PlainTextPassword))
import Data.Qualified
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI (URI)
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified System.CryptoBox as CBox
import Wire.API.User.Client (ClientClass (DesktopClient), ClientType (PermanentClientType), NewClient (..), clientId)
import Wire.API.User.Client.Prekey (Prekey (Prekey), lastPrekey)
import Wire.API.User.Search
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Connection as Connection
import qualified Wire.CLI.Conv as Conv
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.Display (Display)
import qualified Wire.CLI.Display as Display
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Message as Message
import qualified Wire.CLI.Notification as Notification
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.CLI.UUIDGen (UUIDGen)
import qualified Wire.CLI.User as User
import Wire.CLI.Chan (ReadChan)

executeAndPrint :: Members (Display ': ExecuteEffects) r => Opts.Command a -> Sem r ()
executeAndPrint cmd = case cmd of
  Opts.Login _ -> Display.login =<< execute cmd
  Opts.ListConvs -> Display.listConvs =<< execute cmd
  Opts.ListMessages _ -> Display.listMessages =<< execute cmd
  Opts.Search _ -> Display.search =<< execute cmd
  Opts.ListConnections _ -> Display.listConnections =<< execute cmd
  Opts.GetSelf _ -> Display.showSelfUser =<< execute cmd
  --
  Opts.Logout -> execute cmd
  Opts.SyncConvs -> execute cmd
  Opts.SyncNotifications -> execute cmd
  Opts.RegisterWireless _ -> execute cmd
  Opts.RequestActivationCode _ -> execute cmd
  Opts.Register _ -> execute cmd
  Opts.SetHandle _ -> execute cmd
  Opts.SyncConnections -> execute cmd
  Opts.UpdateConnection _ -> execute cmd
  Opts.Connect _ -> execute cmd
  Opts.SendMessage _ -> execute cmd
  Opts.RefreshToken -> execute cmd
  Opts.SyncSelf -> execute cmd
  Opts.WatchNotifications -> execute cmd

type ExecuteEffects = '[Backend, Store, CryptoBox, Random, UUIDGen, Error WireCLIError, ReadChan]

execute :: Members ExecuteEffects r => Opts.Command a -> Sem r a
execute = \case
  Opts.Login loginOpts -> performLogin loginOpts
  Opts.Logout -> error "Not implemented"
  Opts.SyncConvs -> Conv.sync
  Opts.ListConvs -> Conv.list
  Opts.SyncNotifications -> Notification.sync
  Opts.RegisterWireless opts -> performWirelessRegister opts
  Opts.Search opts -> search opts
  Opts.RequestActivationCode opts -> Backend.requestActivationCode opts
  Opts.Register opts -> Backend.register opts >>= getTokenAndRegisterClient (Opts.registerServer opts)
  Opts.SetHandle handle -> performSetHandle handle
  Opts.SyncConnections -> Connection.sync
  Opts.ListConnections opts -> Connection.list opts
  Opts.UpdateConnection opts -> Connection.update opts
  Opts.Connect cr -> connect cr
  Opts.SendMessage opts -> Message.send opts
  Opts.ListMessages (Opts.ListMessagesOptions conv n) -> Store.getLastNMessages conv n
  Opts.RefreshToken -> refreshTokenAndSave
  Opts.SyncSelf -> void User.syncSelf
  Opts.GetSelf opts -> User.getSelf opts
  Opts.WatchNotifications -> Notification.watch

refreshTokenAndSave :: Members '[Backend, Store, Error WireCLIError] r => Sem r ()
refreshTokenAndSave = do
  creds <- Store.getCreds >>= Error.note WireCLIError.NotLoggedIn
  let server = Backend.server creds
      cookies = Backend.credentialCookies $ Backend.credential creds
  newCreds <- Backend.refreshToken server cookies
  Store.saveCreds $ Backend.ServerCredential server newCreds

performSetHandle :: Members [Store, Backend, Error WireCLIError] r => Handle -> Sem r ()
performSetHandle handle = do
  creds <- Store.getCreds >>= Error.note WireCLIError.NotLoggedIn
  Backend.setHandle creds handle

performLogin :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Opts.LoginOptions -> Sem r (Maybe Text)
performLogin opts = do
  res <- Backend.login opts
  case res of
    Backend.LoginFailure e -> pure $ Just e
    Backend.LoginSuccess t -> do
      let serverCred = Backend.ServerCredential (Opts.loginServer opts) t
      Store.saveCreds serverCred
      mSavedClient <- Store.getClientId
      when (isNothing mSavedClient) $
        registerClient serverCred (Opts.loginPassword opts)
      pure Nothing

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
  cred <- Backend.refreshToken server cookies
  let serverCred = Backend.ServerCredential server cred
  Store.saveCreds serverCred
  -- Untested random password generation
  password <- Text.pack <$> replicateM 15 (Random.randomR ('a', 'z'))
  registerClient serverCred password

registerClient :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Backend.ServerCredential -> Text -> Sem r ()
registerClient serverCred password = do
  preKeys <- mapM (throwCBoxError <=< CryptoBox.newPrekey) [0 .. 99]
  Prekey _ pkLast <- throwCBoxError =<< CryptoBox.newPrekey maxBound
  let newClient =
        NewClient
          { newClientPrekeys = preKeys,
            newClientLastKey = lastPrekey pkLast,
            newClientType = PermanentClientType,
            newClientLabel = Just "wire-cli",
            newClientClass = Just DesktopClient,
            newClientCookie = Just "wire-cli-cookie-label",
            newClientPassword = Just $ PlainTextPassword password,
            newClientModel = Just "wire-cli",
            newClientCapabilities = Nothing
          }
  registeredClient <- Backend.registerClient serverCred newClient
  Store.saveClientId (clientId registeredClient)

search :: Members '[Backend, Store, Error WireCLIError] r => Opts.SearchOptions -> Sem r (SearchResult Contact)
search opts = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  Backend.search serverCreds opts

connect :: Members '[Backend, Store, Error WireCLIError] r => Qualified UserId -> Sem r ()
connect quid = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  Backend.connect serverCreds quid
