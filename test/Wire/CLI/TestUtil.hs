module Wire.CLI.TestUtil where

import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Random (Random)
import qualified Polysemy.Random as Random
import Test.Hspec
import Test.Polysemy.Mock
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
  Members [MockImpl Store IO, Embed IO] r =>
  Sem (Error WireCLIError ': r) () ->
  Sem r ()
assertNoUnauthenticatedAccess action = do
  Store.mockGetCredsReturns (pure Nothing)

  eitherErr <- Error.runError action

  embed $ eitherErr `shouldBe` Left WireCLIError.NotLoggedIn
