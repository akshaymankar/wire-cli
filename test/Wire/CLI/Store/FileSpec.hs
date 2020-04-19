module Wire.CLI.Store.FileSpec where

import Control.Carrier.Lift (runM, sendM)
import qualified Data.Aeson as Aeson
import qualified System.IO.Temp as Temp
import Test.Hspec
import Test.QuickCheck
import qualified Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Store as Store
import qualified Wire.CLI.Store.File as FileStore

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = describe "Store.File" $ do
  it "should save creds" testSaveCreds

testSaveCreds :: IO ()
testSaveCreds =
  inTestDir $ \path ->
    runM . FileStore.run path $ do
      cred <- sendM $ generate arbitrary
      -- store the cred
      Store.saveCreds cred
      -- read from file and expect same
      decodedCred <- sendM $ Aeson.decodeFileStrict (path <> "/credential.json")
      sendM $ decodedCred `shouldBe` Just cred

inTestDir :: (FilePath -> IO a) -> IO a
inTestDir = Temp.withSystemTempDirectory "test"
