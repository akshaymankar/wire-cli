module Wire.CLI.TestUtil where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import Test.Hspec
import Test.Polysemy.Mock
import Wire.CLI.Chan (ReadChan, WriteChan)
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Mocks.Store as Store
import Wire.CLI.Store (Store)

assertNoError :: (HasCallStack, Member (Embed IO) r, Show e) => Sem (Error e ': r) a -> Sem r a
assertNoError s = do
  eitherErr <- Error.runError s
  case eitherErr of
    Left e -> do
      embed $ expectationFailure $ "Expected No Error, but got error: " <> show e
      error "Impossible!!"
    Right a -> pure a

assertNoRandomness :: (HasCallStack, Member (Embed IO) r) => Sem (Random ': r) a -> Sem r a
assertNoRandomness = interpret $ \case
  Random.Random -> do
    embed $ expectationFailure "Expected no random generation, but got one invocation of `random`"
    error "Impossible!!"
  Random.RandomR _ -> do
    embed $ expectationFailure "Expected no random generation, but got one invotaion of `randomR`"
    error "Impossible!!"

assertNoUnauthenticatedAccess ::
  (Members [MockImpl Store IO, Embed IO] r, HasCallStack) =>
  Sem (Error WireCLIError ': r) a ->
  Sem r ()
assertNoUnauthenticatedAccess action = do
  Store.mockGetCredsReturns (pure Nothing)

  eitherErr <- Error.runError action

  embed $ case eitherErr of
    Left WireCLIError.NotLoggedIn -> pure ()
    Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
    Right _ -> expectationFailure "Expected error, got none"

assertNoChan :: Member (Embed IO) r => Sem (ReadChan ': WriteChan ': r) a -> Sem r a
assertNoChan = assertNoWriteChan . assertNoReadChan

assertNoReadChan :: Member (Embed IO) r => Sem (ReadChan ': r) a -> Sem r a
assertNoReadChan =
  interpret $ \_ -> embed $ do
    expectationFailure "Unexpected ReadChan"
    error "Impossible!"

assertNoWriteChan :: Member (Embed IO) r => Sem (WriteChan ': r) a -> Sem r a
assertNoWriteChan =
  interpret $ \_ -> embed $ do
    expectationFailure "Unexpected WriteChan"
    error "Impossible!"

assertLookup :: (Ord k, Show k, MonadIO m, HasCallStack) => k -> Map.Map k v -> m v
assertLookup k m = do
  liftIO $ Map.keys m `shouldContain` [k]
  pure $ (Map.!) m k

assertLookup2 ::
  (Ord k1, Show k1, Ord k2, Show k2, MonadIO m, HasCallStack) =>
  k1 ->
  k2 ->
  Map.Map k1 (Map.Map k2 v) ->
  m v
assertLookup2 k1 k2 m = do
  assertLookup k1 m >>= assertLookup k2

assertLookup3 ::
  (Ord k1, Show k1, Ord k2, Show k2, Ord k3, Show k3, MonadIO m, HasCallStack) =>
  k1 ->
  k2 ->
  k3 ->
  Map.Map k1 (Map.Map k2 (Map.Map k3 v)) ->
  m v
assertLookup3 k1 k2 k3 m = do
  assertLookup2 k1 k2 m >>= assertLookup k3
