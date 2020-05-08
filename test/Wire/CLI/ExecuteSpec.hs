{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.ExecuteSpec where

import qualified Network.URI as URI
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.Polysemy.Mock
import Test.Polysemy.Mock.TH
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Backend.Client as Client
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.Execute as Execute
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)

genMock ''Backend

genMock ''Store

genMock ''CryptoBox

assertNoError :: (HasCallStack, Member (Embed IO) r, Show e) => Sem (Error e ': r) a -> Sem r a
assertNoError s = do
  eitherErr <- Error.runError s
  case eitherErr of
    Left e -> do
      embed $ expectationFailure $ "Expected No Error, but got error: " <> show e
      error "Impossible!!"
    Right a -> pure a

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = do
  describe "Execute" $ do
    it "should login and store creds" testLoginAndStore

testLoginAndStore :: IO ()
testLoginAndStore = runM . evalMocks @'[Backend, Store, CryptoBox] $ do
  let Just server = URI.parseURI "https://be.example.com"
  let loginOpts = Opts.LoginOptions server "handle" "pwwpw"
  let loginCommand = Opts.Login loginOpts
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
  embed $ saveCredsCalls' `shouldBe` [cred]
  -- Register Client
  -- Generate 100 prekeys
  newPrekeyCalls' <- mockNewPrekeyCalls
  embed $ length newPrekeyCalls' `shouldBe` 101
  embed $ newPrekeyCalls' `shouldBe` ([0 .. 99] <> [maxBound])
  -- Make register call to backend
  registerClientCalls' <- mockRegisterClientCalls
  embed $ length registerClientCalls' `shouldBe` 1
  let (registerCred, uri, newClient) = head registerClientCalls'
  embed $ registerCred `shouldBe` cred
  embed $ uri `shouldBe` server
  embed $ Client.cookie newClient `shouldBe` "wire-cli-cookie-label"
  embed $ Client.password newClient `shouldBe` "pwwpw"
  embed $ Client.model newClient `shouldBe` "wire-cli"
  embed $ Client.clas newClient `shouldBe` Client.Desktop
  embed $ Client.typ newClient `shouldBe` Client.Permanent
  embed $ Client.label newClient `shouldBe` "wire-cli"
  embed $ length (Client.prekeys newClient) `shouldBe` 100
