module Wire.CLI.ExecuteSpec where

import Control.Carrier.Lift (runM, sendM)
import qualified Network.URI as URI
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.QuickCheck
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Backend.Client as Client
import qualified Wire.CLI.Execute as Execute
import qualified Wire.CLI.MockBackend as MockBackend
import qualified Wire.CLI.MockCryptoBox as MockCryptoBox
import qualified Wire.CLI.MockStore as MockStore
import qualified Wire.CLI.Options as Opts

withMocks :: MockBackend.BackendC (MockStore.StoreC (MockCryptoBox.CryptoBoxC m)) a -> m a
withMocks = MockCryptoBox.mock . MockStore.mock . MockBackend.mock

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = do
  describe "Execute" $ do
    it "should login and store creds" testLoginAndStore

testLoginAndStore :: IO ()
testLoginAndStore = runM . MockBackend.run . MockStore.run . MockCryptoBox.run $ do
  let Just server = URI.parseURI "https://be.example.com"
  let loginOpts = Opts.LoginOptions server "handle" "pwwpw"
  let loginCommand = Opts.Login loginOpts
  cred <- sendM $ generate arbitrary
  prekey <- sendM $ generate arbitrary
  MockBackend.mockLoginReturns (const $ Backend.LoginSuccess cred)
  MockCryptoBox.mockNewPrekeyReturns (const $ CBox.Success prekey)
  -- execute the command
  withMocks $ Execute.execute loginCommand
  -- expectations
  -- 1. Call backend
  loginCalls <- MockBackend.mockLoginCalls
  sendM $ loginCalls `shouldBe` [loginOpts]
  -- 2. Store creds
  saveCredsCalls <- MockStore.mockSaveCredsCalls
  sendM $ saveCredsCalls `shouldBe` [cred]
  -- 3. Register Client
  -- 3.1 Generate 100 prekeys
  newPrekeyCalls <- MockCryptoBox.mockNewPrekeyCalls
  sendM $ length newPrekeyCalls `shouldBe` 101
  sendM $ newPrekeyCalls `shouldBe` ([0 .. 99] <> [maxBound])
  -- 3.2 Make register call to backend
  registerClientCalls <- MockBackend.mockRegisterClientCalls
  sendM $ length registerClientCalls `shouldBe` 1
  let (registerCred, uri, newClient) = head registerClientCalls
  sendM $ do
    registerCred `shouldBe` cred
    uri `shouldBe` server
    Client.cookie newClient `shouldBe` "wire-cli-cookie-label"
    Client.password newClient `shouldBe` "pwwpw"
    Client.model newClient `shouldBe` "wire-cli"
    Client.clas newClient `shouldBe` Client.Desktop
    Client.typ newClient `shouldBe` Client.Permanent
    Client.label newClient `shouldBe` "wire-cli"
    length (Client.prekeys newClient) `shouldBe` 100
