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

import Data.Text
import Polysemy
import Polysemy.State
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

data MockStore m a where
  MockSaveCreds :: Text -> MockStore m ()
  MockSaveCredsReturns :: (Text -> ()) -> MockStore m ()
  MockSaveCredsCalls :: MockStore m [Text]

makeSem ''MockStore

data MockStoreState = MockStoreState {scCalls :: [Text], scReturns :: Text -> ()}

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
