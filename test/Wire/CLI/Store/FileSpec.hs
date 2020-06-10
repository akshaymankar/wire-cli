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
  describePersistence "creds" "credential.json" Store.saveCreds Store.getCreds

  describePersistence "convs" "conversations.json" Store.saveConvs Store.getConvs

  describePersistence "client-id" "client-id.json" Store.saveClientId Store.getClientId

  describePersistence "last notification" "last-notification-id.json" Store.saveLastNotificationId Store.getLastNotificationId

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

shoudlGetNonExistent :: (Eq a, Show a) => Sem '[Store, Embed IO] (Maybe a) -> IO ()
shoudlGetNonExistent getFn = inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
  thing <- getFn
  embed $ thing `shouldBe` Nothing

describePersistence :: (Eq a, Show a, ToJSON a, FromJSON a, Arbitrary a) => String -> FilePath -> (a -> Sem '[Store, Embed IO] ()) -> Sem '[Store, Embed IO] (Maybe a) -> Spec
describePersistence name f saveFn getFn = describe name $ do
  it "can be saved" $
    saveFn `shouldSaveTo` f

  it "can be retrieved" $
    getFn `shouldGetSavedBy` saveFn

  it "gracefully denies retrieval when not previously saved" $
    shoudlGetNonExistent getFn
