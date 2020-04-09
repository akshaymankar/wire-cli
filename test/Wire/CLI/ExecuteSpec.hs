module Wire.CLI.ExecuteSpec where

import qualified Network.URI as URI
import Polysemy
import Test.Hspec
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Execute as Execute
import qualified Wire.CLI.MockBackend as MockBackend
import Wire.CLI.MockBackend (MockBackend)
import qualified Wire.CLI.MockStore as MockStore
import Wire.CLI.MockStore (MockStore)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)

withMocks ::
  Members '[MockStore, MockBackend, Embed IO] r =>
  Sem (Backend ': (Store ': r)) a ->
  Sem r a
withMocks = MockStore.mock . MockBackend.mock

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = do
  describe "Execute" $ do
    it "should login and store creds" testLoginAndStore

testLoginAndStore :: IO ()
testLoginAndStore = runM . MockBackend.run . MockStore.run $ do
  let Just server = URI.parseURI "https://be.example.com"
  let loginOpts = Opts.LoginOptions server "handle" "pwwpw"
  let loginCommand = Opts.Login loginOpts
  cred <- embed $ generate arbitrary
  MockBackend.mockLoginReturns (const $ Backend.LoginSuccess cred)
  -- execute the command
  withMocks $ Execute.execute loginCommand
  -- expectations
  loginCalls <- MockBackend.mockLoginCalls
  embed $ loginCalls `shouldBe` [loginOpts]
  saveCredsCalls <- MockStore.mockSaveCredsCalls
  embed $ saveCredsCalls `shouldBe` [cred]
