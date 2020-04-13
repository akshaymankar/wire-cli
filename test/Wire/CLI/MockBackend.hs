{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.MockBackend
  ( MockBackend,
    mockLogin,
    mockLoginCalls,
    mockLoginReturns,
    mockRegisterClient,
    mockRegisterClientCalls,
    mockRegisterClientReturns,
    mock,
    Wire.CLI.MockBackend.run,
  )
where

import Network.URI (URI)
import Polysemy
import Polysemy.State
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Options as Opts

data MockBackend m a where
  MockLogin :: Opts.LoginOptions -> MockBackend m Backend.LoginResponse
  MockLoginCalls :: MockBackend m [Opts.LoginOptions]
  MockLoginReturns :: (Opts.LoginOptions -> Backend.LoginResponse) -> MockBackend m ()
  MockRegisterClient :: Backend.Credential -> URI -> Backend.NewClient -> MockBackend m ()
  MockRegisterClientCalls :: MockBackend m [(Backend.Credential, URI, Backend.NewClient)]
  MockRegisterClientReturns :: (Backend.Credential -> URI -> Backend.NewClient -> ()) -> MockBackend m ()

makeSem ''MockBackend

data MockBackendState
  = MockBackendState
      { loginCalls :: [Opts.LoginOptions],
        loginReturns :: Opts.LoginOptions -> Backend.LoginResponse,
        registerClientCalls :: [(Backend.Credential, URI, Backend.NewClient)],
        registerClientReturns :: Backend.Credential -> URI -> Backend.NewClient -> ()
      }

initialMockBackendState :: MockBackendState
initialMockBackendState =
  MockBackendState
    { loginCalls = [],
      loginReturns = const (error "login calls not mocked yet"),
      registerClientCalls = [],
      registerClientReturns = const (error "register client not mocked yet")
    }

run :: Sem (MockBackend ': r) a -> Sem r a
run = evalState initialMockBackendState . mockBackendToState

mockBackendToState :: forall r a. Sem (MockBackend ': r) a -> Sem (State MockBackendState ': r) a
mockBackendToState = reinterpret $ \case
  MockLogin opts -> do
    state <- get
    put (state {loginCalls = loginCalls state <> [opts]})
    pure $ loginReturns state opts
  MockLoginCalls ->
    loginCalls <$> get
  MockLoginReturns f -> do
    state <- get
    put $ state {loginReturns = f}
  MockRegisterClient cred uri client -> do
    state <- get
    put (state {registerClientCalls = registerClientCalls state <> [(cred, uri, client)]})
    pure $ registerClientReturns state cred uri client
  MockRegisterClientCalls ->
    registerClientCalls <$> get
  MockRegisterClientReturns f -> do
    state <- get
    put $ state {registerClientReturns = f}

mock :: Member MockBackend r => Sem (Backend ': r) a -> Sem r a
mock = interpret $ \case
  Backend.Login opts -> mockLogin opts
  Backend.RegisterClient cred uri client -> mockRegisterClient cred uri client
