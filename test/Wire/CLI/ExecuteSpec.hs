module Wire.CLI.ExecuteSpec where

import qualified Network.URI as URI
import Polysemy
import qualified Polysemy.Error as Error
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Backend.Client as Client
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Execute as Execute
import Wire.CLI.Mocks
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import Wire.CLI.TestUtil

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = do
  describe "Execute Login" $ do
    let Just server = URI.parseURI "https://be.example.com"
    let loginOpts = Opts.LoginOptions server "handle" "pwwpw"
    let loginCommand = Opts.Login loginOpts
    it "should login and store creds" $ runM . evalMocks @'[Backend, Store, CryptoBox] $ do
      cred <- embed $ generate arbitrary
      prekey <- embed $ generate arbitrary
      mockLoginReturns (const $ pure $ Backend.LoginSuccess cred)
      mockNewPrekeyReturns (const . pure $ CBox.Success prekey)
      -- execute the command
      mockMany @'[Backend, Store, CryptoBox] . assertNoError $
        Execute.execute loginCommand
      -- withMocks $ Execute.execute loginCommand
      -- expectations
      -- Call backend
      loginCalls' <- mockLoginCalls
      embed $ loginCalls' `shouldBe` [loginOpts]
      -- Store creds
      saveCredsCalls' <- mockSaveCredsCalls
      embed $ saveCredsCalls' `shouldBe` [Backend.ServerCredential server cred]
      -- Register Client
      -- Generate 100 prekeys
      newPrekeyCalls' <- mockNewPrekeyCalls
      embed $ length newPrekeyCalls' `shouldBe` 101
      embed $ newPrekeyCalls' `shouldBe` ([0 .. 99] <> [maxBound])
      -- Make register call to backend
      registerClientCalls' <- mockRegisterClientCalls
      embed $ length registerClientCalls' `shouldBe` 1
      let (serverCred, newClient) = head registerClientCalls'
      embed $ serverCred `shouldBe` Backend.ServerCredential server cred
      embed $ Client.cookie newClient `shouldBe` "wire-cli-cookie-label"
      embed $ Client.password newClient `shouldBe` "pwwpw"
      embed $ Client.model newClient `shouldBe` "wire-cli"
      embed $ Client.clas newClient `shouldBe` Client.Desktop
      embed $ Client.typ newClient `shouldBe` Client.Permanent
      embed $ Client.label newClient `shouldBe` "wire-cli"
      embed $ length (Client.prekeys newClient) `shouldBe` 100

    it "should error when login fails" $ runM . evalMocks @'[Backend, Store, CryptoBox] $ do
      mockLoginReturns (const $ pure $ Backend.LoginFailure "something failed")

      eitherErr <-
        mockMany @'[Backend, Store, CryptoBox] . Error.runError $
          Execute.execute loginCommand

      embed $ eitherErr `shouldBe` Left (WireCLIError.LoginFailed "something failed")

  describe "Execute SyncConvs" $ do
    it "should get convs from the server and store them" $ runM . evalMocks @'[Backend, Store, CryptoBox] $ do
      convs <- embed $ generate arbitrary
      creds <- embed $ generate arbitrary
      mockGetCredsReturns $ pure (Just creds)
      mockListConvsReturns $ \_ _ _ -> do
        -- c `shouldBe` creds
        pure $ Backend.Convs convs False
      mockMany @'[Backend, Store, CryptoBox] . assertNoError $
        Execute.execute Opts.SyncConvs
      saveConvs <- mockSaveConvsCalls
      embed $ saveConvs `shouldBe` [convs]
