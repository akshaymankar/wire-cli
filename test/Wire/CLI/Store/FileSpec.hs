module Wire.CLI.Store.FileSpec where

import qualified Data.Aeson as Aeson
import Polysemy
import qualified System.IO.Temp as Temp
import Test.Hspec
import Test.QuickCheck
import qualified Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Store as Store
import qualified Wire.CLI.Store.File as FileStore

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = describe "Store.File" $ do
  it "should save creds" $ inTestDir $ \path ->
    runM . FileStore.run path $ do
      cred <- embed $ generate arbitrary

      Store.saveCreds cred

      decodedCred <- embed $ Aeson.eitherDecodeFileStrict (path <> "/credential.json")
      embed $ decodedCred `shouldBe` Right cred
  it "should get creds" $ inTestDir $ \path ->
    runM . FileStore.run path $ do
      cred <- embed $ generate arbitrary

      Store.saveCreds cred
      retrievedCred <- Store.getCreds

      embed $ retrievedCred `shouldBe` Just cred
  it "should save convs" $ inTestDir $ \path ->
    runM . FileStore.run path $ do
      convs <- embed $ generate arbitrary

      Store.saveConvs convs

      decodedConvs <- embed $ Aeson.eitherDecodeFileStrict (path <> "/conversations.json")
      embed $ decodedConvs `shouldBe` Right convs

inTestDir :: (FilePath -> IO a) -> IO a
inTestDir = Temp.withSystemTempDirectory "test"
