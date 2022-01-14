{-# LANGUAGE DeriveFunctor #-}

module Wire.GUI.Worker where

import Control.Concurrent.Chan.Unagi (InChan, OutChan)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Exception (SomeException, try)
import qualified Network.HTTP.Client as HTTP
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend.HTTP as HTTPBackend
import Wire.CLI.Chan (ReadChan)
import qualified Wire.CLI.Chan as Chan
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Execute (execute)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store.File as FileStore
import Wire.CLI.UUIDGen (UUIDGen)
import qualified Wire.CLI.UUIDGen as UUIDGen

type AllEffects = '[CryptoBox, Store, Backend, Random, UUIDGen, Error WireCLIError, ReadChan, Embed IO, Final IO]

data WorkResult a
  = WorkResultError WireCLIError
  | WorkResultException SomeException
  | WorkResultSuccess a
  deriving (Show, Functor)

{- ORMOLU_DISABLE -}
instance Applicative WorkResult where
  pure = WorkResultSuccess

  WorkResultError wce    <*> _                      = WorkResultError wce
  WorkResultException se <*> _                      = WorkResultException se
  _                      <*> WorkResultError wce    = WorkResultError wce
  _                      <*> WorkResultException se = WorkResultException se
  WorkResultSuccess fab  <*> WorkResultSuccess a    = WorkResultSuccess (fab a)
{- ORMOLU_ENABLE -}

workResultToEither :: String -> WorkResult a -> Either String a
workResultToEither prefix =
  \case
    WorkResultError wce -> Left $ prefix <> show wce
    WorkResultException se -> Left $ prefix <> show se
    WorkResultSuccess a -> Right a

tryWorkResult :: Member (Error String) r => String -> WorkResult a -> Sem r a
tryWorkResult prefix = Error.fromEither . workResultToEither prefix

data Work where
  Work :: Sem AllEffects a -> (WorkResult a -> IO ()) -> Work

-- TODO: Wrap Unagi Chan stuff in its own effects
worker :: HTTP.Manager -> FilePath -> CBox.Box -> OutChan Work -> IO ()
worker mgr storePath cbox workChan = go
  where
    runAllEffects :: Sem AllEffects a -> IO (Either WireCLIError a)
    runAllEffects action = do
      runFinal
        . embedToFinal
        . Chan.runRead
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
          runAllEffects $ execute Opts.RefreshToken >> action
        x -> pure x

    go :: IO ()
    go = do
      putStrLn "Waiting For work!"
      Work action callback <- Unagi.readChan workChan
      putStrLn "Got work!"
      callback =<< makeRes (runAllEffectsWithRetry action)
      go

    makeRes :: IO (Either WireCLIError a) -> IO (WorkResult a)
    makeRes action =
      try action >>= \case
        Left exc -> pure (WorkResultException exc)
        Right (Left err) -> pure (WorkResultError err)
        Right (Right a) -> pure (WorkResultSuccess a)

-- | Blocks until work is done. This is required so UI code isn't cluttered with
-- information on how to resolve all the effects.
runActionSync :: InChan Work -> Sem AllEffects a -> IO (WorkResult a)
runActionSync workChan action = do
  -- TODO: Use an MVar
  (inSyncChan, outSyncChan) <- Unagi.newChan
  Unagi.writeChan workChan $ Work action (Unagi.writeChan inSyncChan)
  Unagi.readChan outSyncChan

queueCommand :: InChan Work -> Opts.Command a -> (WorkResult a -> IO ()) -> IO ()
queueCommand workChan cmd = queueAction workChan (execute cmd)

queueAction :: InChan Work -> Sem AllEffects a -> (WorkResult a -> IO ()) -> IO ()
queueAction workChan action callback = Unagi.writeChan workChan $ Work action callback
