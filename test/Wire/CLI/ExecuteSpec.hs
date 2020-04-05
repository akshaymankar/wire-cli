{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.ExecuteSpec where

import Control.Monad (void)
import Data.Text (Text)
import Polysemy
import Polysemy.State
import Test.Hspec
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Execute as Execute
import qualified Wire.CLI.MockBackend as MockBackend
import Wire.CLI.MockBackend (MockBackend)
import qualified Wire.CLI.MockStore as MockStore
import Wire.CLI.MockStore (MockStore)
import qualified Wire.CLI.Options as Opts
import qualified Wire.CLI.Store as Store
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
  let loginOpts =
        Opts.LoginOptions "https://be.example.com" "handle" "pwwpw"
      loginCommand =
        Opts.Login loginOpts
  MockBackend.mockLoginReturns (const $ Backend.LoginSuccess "token")
  --
  withMocks $ Execute.execute loginCommand
  --
  loginCalls <- MockBackend.mockLoginCalls
  embed $ loginCalls `shouldBe` [loginOpts]
  saveCredsCalls <- MockStore.mockSaveCredsCalls
  embed $ saveCredsCalls `shouldBe` ["token"]
  pure ()
