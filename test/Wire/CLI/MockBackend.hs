{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.MockBackend
  ( MockBackend,
    mockLogin,
    mockLoginCalls,
    mockLoginReturns,
    mock,
    Wire.CLI.MockBackend.run,
  )
where

import Polysemy
import Polysemy.State
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Options as Opts

data MockBackend m a where
  MockLogin :: Opts.LoginOptions -> MockBackend m Backend.LoginResponse
  MockLoginCalls :: MockBackend m [Opts.LoginOptions]
  MockLoginReturns :: (Opts.LoginOptions -> Backend.LoginResponse) -> MockBackend m ()

makeSem ''MockBackend

data MockBackendState
  = MockBackendState
      { loginCalls :: [Opts.LoginOptions],
        loginReturns :: Opts.LoginOptions -> Backend.LoginResponse
      }

initialMockBackendState :: MockBackendState
initialMockBackendState = MockBackendState {loginCalls = [], loginReturns = const (error "login calls not mocked yet")}

run :: Sem (MockBackend ': r) a -> Sem r a
run = evalState initialMockBackendState . mockBackendToState

mockBackendToState :: forall r a. Sem (MockBackend ': r) a -> Sem (State MockBackendState ': r) a
mockBackendToState = reinterpret $ \case
  MockLogin opts -> do
    MockBackendState lc lr <- get
    put (MockBackendState (lc <> [opts]) lr)
    pure $ lr opts
  MockLoginCalls -> do
    MockBackendState lc _ <- get
    pure lc
  MockLoginReturns lr -> do
    beState <- get
    put $ beState {loginReturns = lr}

mock :: Member MockBackend r => Sem (Backend ': r) a -> Sem r a
mock = interpret $ \case
  Backend.Login opts -> mockLogin opts
