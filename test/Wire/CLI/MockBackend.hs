{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Wire.CLI.MockBackend
  ( MockBackend,
    BackendC,
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

import Control.Algebra
import Control.Carrier.State.Strict (StateC, evalState, get, put)
import Network.URI (URI)
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

mockLogin :: Has MockBackend sig m => Opts.LoginOptions -> m Backend.LoginResponse
mockLogin = send . MockLogin

mockLoginCalls :: Has MockBackend sig m => m [Opts.LoginOptions]
mockLoginCalls = send MockLoginCalls

mockLoginReturns :: Has MockBackend sig m => (Opts.LoginOptions -> Backend.LoginResponse) -> m ()
mockLoginReturns = send . MockLoginReturns

mockRegisterClient :: Has MockBackend sig m => Backend.Credential -> URI -> Backend.NewClient -> m ()
mockRegisterClient cred uri client = send $ MockRegisterClient cred uri client

mockRegisterClientCalls :: Has MockBackend sig m => m [(Backend.Credential, URI, Backend.NewClient)]
mockRegisterClientCalls = send MockRegisterClientCalls

mockRegisterClientReturns :: Has MockBackend sig m => (Backend.Credential -> URI -> Backend.NewClient -> ()) -> m ()
mockRegisterClientReturns = send . MockRegisterClientReturns

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

run :: Functor m => MockBackendC m a -> m a
run = evalState initialMockBackendState . runMockBackendC

mockBackendToState :: (Algebra sig m) => MockBackend n a -> StateC MockBackendState m a
mockBackendToState = \case
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

interpretToMock :: (Has MockBackend sig m2) => Backend m1 a -> m2 a
interpretToMock = \case
  Backend.Login opts -> mockLogin opts
  Backend.RegisterClient cred uri client -> mockRegisterClient cred uri client

newtype MockBackendC m a = MockBackendC {runMockBackendC :: StateC MockBackendState m a}
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (MockBackend :+: sig) (MockBackendC m) where
  alg hdl sig ctx =
    MockBackendC $
      case sig of
        L mb -> (<$ ctx) <$> mockBackendToState mb
        R other -> alg (runMockBackendC . hdl) (R other) ctx

newtype BackendC m a = BackendC {mock :: m a}
  deriving (Functor, Applicative, Monad)

instance (Has MockBackend sig m, Algebra sig m) => Algebra (Backend :+: sig) (BackendC m) where
  alg hdl sig ctx =
    BackendC $
      case sig of
        L b -> (<$ ctx) <$> interpretToMock b
        R other -> alg (mock . hdl) other ctx
