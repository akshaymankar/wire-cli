module Wire.CLI.Store.FileSpec where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Polysemy
import qualified System.IO.Temp as Temp
import Test.Hspec
import Test.QuickCheck
import qualified Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import qualified Wire.CLI.Store.File as FileStore

spec :: Spec
spec = describe "Store.File" $ do
  it "should save creds" $
    Store.saveCreds `shouldSaveTo` "/credential.json"

  it "should get creds" $
    Store.getCreds `shouldGetSavedBy` Store.saveCreds

  it "should save convs" $
    Store.saveConvs `shouldSaveTo` "conversations.json"

  it "should get convs" $
    Store.getConvs `shouldGetSavedBy` Store.saveConvs

  it "should save client id" $
    Store.saveClientId `shouldSaveTo` "client-id.json"

  it "should get client id" $
    Store.getClientId `shouldGetSavedBy` Store.saveClientId

inTestDir :: (FilePath -> IO a) -> IO a
inTestDir = Temp.withSystemTempDirectory "test"

shouldSaveTo :: (Eq a, Show a, FromJSON a, Arbitrary a) => (a -> Sem '[Store, Embed IO] ()) -> FilePath -> IO ()
shouldSaveTo saveFn file = inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
  thing <- embed $ generate arbitrary
  saveFn thing
  decodedThing <- embed $ Aeson.eitherDecodeFileStrict (baseDir <> "/" <> file)
  embed $ decodedThing `shouldBe` Right thing

shouldGetSavedBy :: (Eq a, Show a, ToJSON a, Arbitrary a) => Sem '[Store, Embed IO] (Maybe a) -> (a -> Sem '[Store, Embed IO] ()) -> IO ()
shouldGetSavedBy getFn saveFn = inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
  thing <- embed $ generate arbitrary
  saveFn thing
  retrievedThing <- getFn
  embed $ retrievedThing `shouldBe` Just thing
