module Wire.CLI.Store.FileSpec where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Json.Util (toUTCTimeMillis)
import qualified Data.Time.Format.ISO8601 as Time
import Polysemy
import qualified System.IO.Temp as Temp
import Test.Hspec
import Test.QuickCheck
import qualified Wire.API.Connection as Connection
import qualified Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.CLI.Store.Arbitrary ()
import qualified Wire.CLI.Store.File as FileStore

spec :: Spec
spec = describe "Store.File" $ do
  describePersistence "creds" "credential.json" Store.saveCreds Store.getCreds

  describePersistence "convs" "conversations.json" Store.saveConvs Store.getConvs

  describePersistence "client-id" "client-id.json" Store.saveClientId Store.getClientId

  describePersistence "last notification" "last-notification-id.json" Store.saveLastNotificationId Store.getLastNotificationId

  describePersistence "self user" "self.json" Store.saveSelf Store.getSelf

  describe "connections" $ do
    it "can be saved" $
      Store.saveConnections `shouldSaveTo` "connections.json"

    it "can be retrieved" $
      (Just <$> Store.getConnections) `shouldGetSavedBy` Store.saveConnections

    it "returns empty list when non previously saved" $
      inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
        conns <- Store.getConnections
        embed $ conns `shouldBe` []

    describe "addConnection" $ do
      it "adds new connections to the end" $
        inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
          firstConn <- embed $ generate arbitrary
          Store.saveConnections [firstConn]
          secondConn <- embed $ generate arbitrary
          Store.addConnection secondConn
          storedConns <- Store.getConnections
          embed $ storedConns `shouldBe` [firstConn, secondConn]

      it "replaces old connections" $
        inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
          conn <- embed $ generate arbitrary

          t1 <- embed $ toUTCTimeMillis <$> Time.iso8601ParseM "1986-11-04T22:19:00Z"
          Store.saveConnections [conn {Connection.ucLastUpdate = t1}]

          t2 <- embed $ toUTCTimeMillis <$> Time.iso8601ParseM "2019-11-04T22:19:00Z"
          let updatedConn = conn {Connection.ucLastUpdate = t2}
          Store.addConnection updatedConn

          storedConns <- Store.getConnections
          embed $ storedConns `shouldBe` [updatedConn]

      it "ignores outdated connections" $
        inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
          conn <- embed $ generate arbitrary

          t1 <- embed $ toUTCTimeMillis <$> Time.iso8601ParseM "1986-11-04T22:19:00Z"
          let conn1 = conn {Connection.ucLastUpdate = t1}
          Store.saveConnections [conn1]

          t2 <- embed $ toUTCTimeMillis <$> Time.iso8601ParseM "1953-11-04T22:19:00Z"
          let outdatedConn = conn {Connection.ucLastUpdate = t2}
          Store.addConnection outdatedConn

          storedConns <- Store.getConnections
          embed $ storedConns `shouldBe` [conn1]

  describe "messages" $ do
    describe "addMessage" $ do
      it "adds new connections to the end" $
        inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
          firstMsg <- embed $ generate arbitrary
          convId <- embed $ generate arbitrary
          Store.addMessage convId firstMsg
          secondMsg <- embed $ generate arbitrary
          Store.addMessage convId secondMsg
          msgs <- Store.getLastNMessages convId 2
          embed $ msgs `shouldBe` [firstMsg, secondMsg]

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

shoudlGetNonExistentList :: (Eq a, Show a) => Sem '[Store, Embed IO] [a] -> IO ()
shoudlGetNonExistentList getFn = inTestDir $ \baseDir -> runM . FileStore.run baseDir $ do
  thing <- getFn
  embed $ thing `shouldBe` []

describePersistence :: (Eq a, Show a, ToJSON a, FromJSON a, Arbitrary a) => String -> FilePath -> (a -> Sem '[Store, Embed IO] ()) -> Sem '[Store, Embed IO] (Maybe a) -> Spec
describePersistence name f saveFn getFn = describe name $ do
  it "can be saved" $
    saveFn `shouldSaveTo` f

  it "can be retrieved" $
    getFn `shouldGetSavedBy` saveFn

  it "gracefully denies retrieval when not previously saved" $
    shoudlGetNonExistent getFn
