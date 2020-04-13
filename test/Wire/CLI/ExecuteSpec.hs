module Wire.CLI.ExecuteSpec where

import qualified Network.URI as URI
import Polysemy
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Backend.Client as Client
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.Execute as Execute
import qualified Wire.CLI.MockBackend as MockBackend
import Wire.CLI.MockBackend (MockBackend)
import Wire.CLI.MockCryptoBox (MockCryptoBox)
import qualified Wire.CLI.MockCryptoBox as MockCryptoBox
import qualified Wire.CLI.MockStore as MockStore
import Wire.CLI.MockStore (MockStore)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)

withMocks ::
  Members '[MockCryptoBox, MockStore, MockBackend, Embed IO] r =>
  Sem (Backend ': (Store ': (CryptoBox ': r))) a ->
  Sem r a
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
  cred <- embed $ generate arbitrary
  prekey <- embed $ generate arbitrary
  MockBackend.mockLoginReturns (const $ Backend.LoginSuccess cred)
  MockCryptoBox.mockNewPrekeyReturns (const $ CBox.Success prekey)
  -- execute the command
  withMocks $ Execute.execute loginCommand
  -- expectations
  -- Call backend
  loginCalls <- MockBackend.mockLoginCalls
  embed $ loginCalls `shouldBe` [loginOpts]
  -- Store creds
  saveCredsCalls <- MockStore.mockSaveCredsCalls
  embed $ saveCredsCalls `shouldBe` [cred]
  -- Register Client
  -- Generate 100 prekeys
  newPrekeyCalls <- MockCryptoBox.mockNewPrekeyCalls
  embed $ length newPrekeyCalls `shouldBe` 101
  embed $ newPrekeyCalls `shouldBe` ([0 .. 99] <> [maxBound])
  -- Make register call to backend
  registerClientCalls <- MockBackend.mockRegisterClientCalls
  embed $ length registerClientCalls `shouldBe` 1
  let (registerCred, uri, newClient) = head registerClientCalls
  embed $ registerCred `shouldBe` cred
  embed $ uri `shouldBe` server
  embed $ Client.cookie newClient `shouldBe` "wire-cli-cookie-label"
  embed $ Client.password newClient `shouldBe` "pwwpw"
  embed $ Client.model newClient `shouldBe` "wire-cli"
  embed $ Client.clas newClient `shouldBe` Client.Desktop
  embed $ Client.typ newClient `shouldBe` Client.Permanent
  embed $ Client.label newClient `shouldBe` "wire-cli"
  embed $ length (Client.prekeys newClient) `shouldBe` 100
