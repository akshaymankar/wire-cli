{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Wire.CLI.MockStore
  ( MockStore,
    StoreC,
    mockSaveCreds,
    mockSaveCredsReturns,
    mockSaveCredsCalls,
    mock,
    Wire.CLI.MockStore.run,
  )
where

import Control.Algebra
import Control.Carrier.State.Strict (StateC, evalState, get, put)
import qualified Wire.CLI.Backend.Credential as Backend
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

data MockStore m a where
  MockSaveCreds :: Backend.Credential -> MockStore m ()
  MockSaveCredsReturns :: (Backend.Credential -> ()) -> MockStore m ()
  MockSaveCredsCalls :: MockStore m [Backend.Credential]

mockSaveCreds :: Has MockStore sig m => Backend.Credential -> m ()
mockSaveCreds = send . MockSaveCreds

mockSaveCredsReturns :: Has MockStore sig m => (Backend.Credential -> ()) -> m ()
mockSaveCredsReturns = send . MockSaveCredsReturns

mockSaveCredsCalls :: Has MockStore sig m => m [Backend.Credential]
mockSaveCredsCalls = send MockSaveCredsCalls

data MockStoreState = MockStoreState {scCalls :: [Backend.Credential], scReturns :: Backend.Credential -> ()}

initialMockStoreState :: MockStoreState
initialMockStoreState =
  MockStoreState {scCalls = [], scReturns = const ()}

mockToState :: (Algebra sig m) => MockStore n a -> StateC MockStoreState m a
mockToState = \case
  MockSaveCreds c -> do
    state <- get
    put state {scCalls = scCalls state <> [c]}
    pure (scReturns state c)
  MockSaveCredsReturns f -> do
    state <- get
    put state {scReturns = f}
  MockSaveCredsCalls -> do
    MockStoreState calls _ <- get
    pure calls

interpretToMock :: (Has MockStore sig m2) => Store m1 a -> m2 a
interpretToMock = \case
  Store.SaveCreds c -> mockSaveCreds c

run :: Functor m => MockStoreC m a -> m a
run = evalState initialMockStoreState . runMockStoreC

newtype MockStoreC m a = MockStoreC {runMockStoreC :: StateC MockStoreState m a}
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (MockStore :+: sig) (MockStoreC m) where
  alg hdl sig ctx =
    MockStoreC $
      case sig of
        L s -> (<$ ctx) <$> mockToState s
        R other -> alg (runMockStoreC . hdl) (R other) ctx

newtype StoreC m a = StoreC {mock :: m a}
  deriving (Functor, Applicative, Monad)

instance (Has MockStore sig m, Algebra sig m) => Algebra (Store :+: sig) (StoreC m) where
  alg hdl sig ctx =
    StoreC $
      case sig of
        L s -> (<$ ctx) <$> interpretToMock s
        R other -> alg (mock . hdl) other ctx
