{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.MockStore
  ( MockStore,
    mockSaveCreds,
    mockSaveCredsReturns,
    mockSaveCredsCalls,
    mock,
    Wire.CLI.MockStore.run,
  )
where

import Polysemy
import Polysemy.State
import qualified Wire.CLI.Backend.Types as Backend
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

data MockStore m a where
  MockSaveCreds :: Backend.Credential -> MockStore m ()
  MockSaveCredsReturns :: (Backend.Credential -> ()) -> MockStore m ()
  MockSaveCredsCalls :: MockStore m [Backend.Credential]

makeSem ''MockStore

data MockStoreState = MockStoreState {scCalls :: [Backend.Credential], scReturns :: Backend.Credential -> ()}

run :: Member (Embed IO) r => Sem (MockStore ': r) a -> Sem r a
run = evalState initialMockStoreState . mockStoreToState
  where
    initialMockStoreState :: MockStoreState
    initialMockStoreState =
      MockStoreState {scCalls = [], scReturns = const ()}
    --
    mockStoreToState :: forall r a. Sem (MockStore ': r) a -> Sem (State MockStoreState ': r) a
    mockStoreToState = reinterpret $ \case
      MockSaveCreds c -> do
        state <- get
        put state {scCalls = scCalls state <> [c]}
        pure $ scReturns state c
      MockSaveCredsReturns f -> do
        state <- get
        put state {scReturns = f}
      MockSaveCredsCalls -> do
        MockStoreState calls _ <- get
        return calls

mock :: Member MockStore r => Sem (Store ': r) a -> Sem r a
mock = interpret $ \case
  Store.SaveCreds c -> mockSaveCreds c
