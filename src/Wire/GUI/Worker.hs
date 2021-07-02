module Wire.GUI.Worker where

import Control.Concurrent.Chan.Unagi (InChan, OutChan)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Network.HTTP.Client as HTTP
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Backend.HTTP as HTTPBackend
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Execute (execute)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import qualified Wire.CLI.Store.File as FileStore
import Wire.CLI.UUIDGen (UUIDGen)
import qualified Wire.CLI.UUIDGen as UUIDGen

type AllEffects = '[CryptoBox, Store, Backend, Random, UUIDGen, Error WireCLIError, Embed IO, Final IO]

-- TODO: Maybe the error should be 'SomeException'
data Work where
  Work :: Sem AllEffects a -> (Either WireCLIError a -> IO ()) -> Work

-- TODO: Wrap Unagi Chan stuff in its own effects
worker :: HTTP.Manager -> FilePath -> CBox.Box -> OutChan Work -> IO ()
worker mgr storePath cbox workChan = go
  where
    runAllEffects :: Sem AllEffects a -> IO (Either WireCLIError a)
    runAllEffects action = do
      runFinal
        . embedToFinal
        . Error.errorToIOFinal -- TODO: log the error!
        . UUIDGen.run
        . Random.runRandomIO
        . HTTPBackend.run "wire-cli-label" mgr
        . FileStore.run storePath
        . CryptoBoxFFI.run cbox
        $ action

    runAllEffectsWithRetry :: Sem AllEffects a -> IO (Either WireCLIError a)
    runAllEffectsWithRetry action = do
      eitherRes <- runAllEffects action
      case eitherRes of
        Left WireCLIError.Http401 ->
          runAllEffects $ refreshTokenAndSave >> action
        x -> pure x

    go :: IO ()
    go = do
      putStrLn "Waiting For work!"
      Work action callback <- Unagi.readChan workChan
      putStrLn "Got work!"
      callback =<< runAllEffectsWithRetry action
      go

-- TODO: Unit test
refreshTokenAndSave :: Members '[Backend, Store, Error WireCLIError, Embed IO] r => Sem r ()
refreshTokenAndSave = do
  embed $ putStrLn "refreshing token!"
  creds <- Store.getCreds >>= Error.note WireCLIError.NotLoggedIn
  let server = Backend.server creds
      cookies = Backend.credentialCookies $ Backend.credential creds
  token <- Backend.refreshToken server cookies
  let serverCred = Backend.ServerCredential server (Backend.Credential cookies token)
  Store.saveCreds serverCred

-- | Blocks until work is done. This is required so UI code isn't cluttered with
-- information on how to resolve all the effects.
runActionSync :: InChan Work -> Sem AllEffects a -> IO (Either WireCLIError a)
runActionSync workChan action = do
  -- TODO: Use an MVar
  (inSyncChan, outSyncChan) <- Unagi.newChan
  Unagi.writeChan workChan $ Work action (Unagi.writeChan inSyncChan)
  Unagi.readChan outSyncChan

queueCommand :: InChan Work -> Opts.Command a -> (Either WireCLIError a -> IO ()) -> IO ()
queueCommand workChan cmd = queueAction workChan (execute cmd)

queueAction :: InChan Work -> Sem AllEffects a -> (Either WireCLIError a -> IO ()) -> IO ()
queueAction workChan action callback = Unagi.writeChan workChan $ Work action callback
