module Wire.CLI.NotificationSpec where

import Control.Exception (Exception, throwIO)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Numeric.Natural (Natural)
import Polysemy
import qualified Polysemy.Error as Error
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import qualified Wire.CLI.App as App
import Wire.CLI.Backend (Backend, ClientId (..), ServerCredential)
import Wire.CLI.Backend.Arbitrary (unprocessedNotification)
import Wire.CLI.Backend.Event (Event)
import qualified Wire.CLI.Backend.Event as Event
import Wire.CLI.Backend.Message (Recipients (Recipients), UserClientMap (UserClientMap))
import Wire.CLI.Backend.Notification
import Wire.CLI.Backend.User (UserId (UserId))
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.CryptoBox.TestUtil
import qualified Wire.CLI.Error as WErr
import qualified Wire.CLI.Message as Message
import Wire.CLI.Mocks.Backend
import Wire.CLI.Mocks.CryptoBox ()
import Wire.CLI.Mocks.Store
import qualified Wire.CLI.Notification as Notification
import Wire.CLI.Store (Store)
import Wire.CLI.Store.StoredMessage (StoredMessage (StoredMessage))
import Wire.CLI.TestUtil
import Wire.CLI.Util.ByteStringJSON (Base64ByteString (Base64ByteString))

type MockedEffects = '[Backend, Store, CryptoBox]

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
{-# ANN spec ("HLint: ignore Reduce duplication" :: String) #-}
spec :: Spec
spec = describe "Notification" $ do
  describe "sync" $ do
    it "should error gracefully when user is not logged in" $
      runM . evalMocks @MockedEffects $
        assertNoUnauthenticatedAccess $
          mockMany @MockedEffects Notification.sync

    it "should error gracefully when a client-id is not found" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        mockGetCredsReturns (pure (Just creds))
        mockGetClientIdReturns (pure Nothing)

        eitherErr <- mockMany @MockedEffects . Error.runError $ Notification.sync

        embed $ eitherErr `shouldBe` Left (WErr.ErrorInvalidState WErr.NoClientFound)

    it "should use nil UUID as last notification id when it doesn't exist in the store" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        client <- embed $ generate arbitrary
        mockGetCredsReturns (pure (Just creds))
        mockGetClientIdReturns (pure (Just client))
        mockGetLastNotificationIdReturns (pure Nothing)
        mockGetNotificationsReturns
          ( \_ _ _ _ ->
              pure (NotificationGapDoesNotExist, Notifications False [])
          )

        mockMany @MockedEffects . assertNoError $ Notification.sync

        getNotifCalls <- mockGetNotificationsCalls
        embed $ getNotifCalls `shouldBe` [(creds, 1000, client, NotificationId UUID.nil)]

    it "should page through notifications" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        client <- embed $ generate arbitrary
        notifsPage1 <- embed $ generate $ listOf unprocessedNotification
        lastNotifPage1 <- embed $ generate unprocessedNotification
        notifsPage2 <- embed $ generate $ listOf unprocessedNotification
        lastNotifPage2 <- embed $ generate unprocessedNotification
        prevNotifId <- embed $ generate arbitrary

        mockGetCredsReturns (pure (Just creds))
        mockGetClientIdReturns (pure (Just client))
        mockGetLastNotificationIdReturns (pure (Just prevNotifId))
        mockGetNotificationsReturns
          ( \_ _ _ n ->
              pure $
                if n == prevNotifId
                  then (NotificationGapDoesNotExist, Notifications True (notifsPage1 ++ [lastNotifPage1]))
                  else (NotificationGapDoesNotExist, Notifications False (notifsPage2 ++ [lastNotifPage2]))
          )

        mockMany @MockedEffects . assertNoError $ Notification.sync

        getNotifCalls <- mockGetNotificationsCalls
        saveNotifIdCalls <- mockSaveLastNotificationIdCalls
        embed $ do
          getNotifCalls
            `shouldBe` [ (creds, 1000, client, prevNotifId),
                         (creds, 1000, client, notificationId lastNotifPage1)
                       ]
          saveNotifIdCalls
            `shouldBe` [ notificationId lastNotifPage1,
                         notificationId lastNotifPage2
                       ]

    describe "processing" $ do
      it "should gracefully handle no notifications" $ do
        runM . evalMocks @MockedEffects $ do
          mockGetCredsReturns (Just <$> generate arbitrary)
          mockGetClientIdReturns (Just <$> generate arbitrary)
          mockGetLastNotificationIdReturns (pure Nothing)

          mockGetNotificationsReturns $
            ( \_ _ _ _ ->
                pure (NotificationGapDoesNotExist, Notifications False [])
            )

          mockMany @MockedEffects . assertNoError $ Notification.sync

      it "should store new connections" $
        runM . evalMocks @MockedEffects $ do
          mockGetCredsReturns (Just <$> generate arbitrary)
          mockGetClientIdReturns (Just <$> generate arbitrary)
          mockGetLastNotificationIdReturns (pure Nothing)

          conn <- embed $ generate arbitrary
          let connEvent = Event.ConnectionEvent conn Nothing Nothing
          let event = Event.EventUser (Event.EventUserConnection connEvent)
          mockGetNotificationsReturns $ oneEvent event

          mockMany @MockedEffects . assertNoError $ Notification.sync

          addConnCalls <- mockAddConnectionCalls
          embed $ addConnCalls `shouldBe` [conn]

      it "should decrypt and store new otr messages" $
        runM . evalMocks @MockedEffects $ do
          mockGetCredsReturns (Just <$> generate arbitrary)
          mockGetClientIdReturns (Just <$> generate arbitrary)
          mockGetLastNotificationIdReturns (pure Nothing)

          -- Users, clients and conv
          (ali, aliClient) <- embed $ generate arbitrary
          bobClient <- embed $ generate arbitrary
          convId <- embed $ generate arbitrary

          -- Initialize the cryptoboxes
          bobBox <- embed $ App.openCBox
          bobKey <- newPrekeyWithBox bobBox 0x1231
          aliBox <- embed $ App.openCBox

          -- Ali encrypts a message for Bob
          encryptionSession <- sessionWithBox aliBox (CBox.SID "ses") bobKey
          let secret = "very secret, much wow"
          encryptedMessage <- encrypt aliBox encryptionSession secret

          -- Mock 'getNotifications'
          let msg = Event.OtrMessage aliClient bobClient encryptedMessage Nothing
          sendingTime <- embed Time.getCurrentTime
          let eventData = Event.EventConvOtrMessageAdd msg
              event = Event.EventConv (Event.ConvEvent convId ali sendingTime eventData)
          mockGetNotificationsReturns $ oneEvent event

          -- Bob syncs her notifications
          mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBox $ Notification.sync

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ addMsgCalls `shouldBe` [(convId, StoredMessage ali aliClient sendingTime secret)]

      -- This test essentially verifies that 'Message.mkRecipient' saves the
      -- session and Notification.addOtrMessage first looks up session in the
      -- box instead of trying to create a new sesion from the message.
      it "should be able to decrypt messages for the sessions which already exist" $ do
        runM . evalMocks @MockedEffects $ do
          -- Users and conversations
          (ali, aliClient) <- embed $ generate arbitrary
          (bob, bobClient) <- embed $ generate arbitrary
          convId <- embed $ generate arbitrary

          -- Initialized the cryptoboxes
          bobBox <- embed $ App.openCBox
          _ <- newPrekeyWithBox bobBox 0x1231
          aliBox <- embed $ App.openCBox
          aliKey <- newPrekeyWithBox aliBox 0x7373

          -- Bob sends the first message to Ali
          bobSesForAli <- sessionWithBox bobBox (Message.mkSessionId ali aliClient) aliKey
          (Base64ByteString keyExchangeMessage) <- encrypt bobBox bobSesForAli "some message for key exchange"

          -- Ali decrypts the message
          (aliSesForBob, _) <- decryptWithBox aliBox (Message.mkSessionId bob bobClient) keyExchangeMessage

          -- Ali uses same session to encrypt the next message for Bob
          let secret = "very secret, much wow"
          encryptedMessage <- encrypt aliBox aliSesForBob secret
          let msg = Event.OtrMessage aliClient bobClient encryptedMessage Nothing
          sendingTime <- embed Time.getCurrentTime

          -- Bob syncs her notification
          mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBox $
            Notification.addOtrMessage convId ali sendingTime msg

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ addMsgCalls `shouldBe` [(convId, StoredMessage ali aliClient sendingTime secret)]

      -- This test verifies that a session is only saved after saving a message
      -- in the store
      -- This still fails with 'DuplicateMessage', not sure why
      xit "should be able to decrypt a message again if saving fails" $ do
        runM . evalMocks @MockedEffects $ do
          -- Users and conversations
          (ali, aliClient) <- embed $ generate arbitrary
          (bob, bobClient) <- embed $ generate arbitrary
          convId <- embed $ generate arbitrary

          -- Initialized the cryptoboxes
          bobBox <- embed $ App.openCBox
          _ <- newPrekeyWithBox bobBox 0x1231
          aliBox <- embed $ App.openCBox
          aliKey <- newPrekeyWithBox aliBox 0x7373

          -- Bob sends the first message to Ali
          bobSesForAli <- sessionWithBox bobBox (Message.mkSessionId ali aliClient) aliKey
          (Base64ByteString keyExchangeMessage) <- encrypt bobBox bobSesForAli "some message for key exchange"

          -- Ali decrypts the message
          (aliSesForBob, _) <- decryptWithBox aliBox (Message.mkSessionId bob bobClient) keyExchangeMessage

          -- Ali uses same session to encrypt the next message for Bob
          let secret = "very secret, much wow"
          encryptedMessage <- encrypt aliBox aliSesForBob secret
          let msg = Event.OtrMessage aliClient bobClient encryptedMessage Nothing
          sendingTime <- embed Time.getCurrentTime

          let failedAttempt = runM . evalMocks @MockedEffects $ do
                -- Mock store to throw error
                mockAddMessageReturns (\_ _ -> throwIO TestException)
                -- Bob syncs her notification
                mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBox $
                  Notification.addOtrMessage convId ali sendingTime msg

          embed $ failedAttempt `shouldThrow` (== TestException)

          -- Should succeed this time
          mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBox $
            Notification.addOtrMessage convId ali sendingTime msg

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ addMsgCalls `shouldBe` [(convId, StoredMessage ali aliClient sendingTime secret)]

oneEvent ::
  Event ->
  (ServerCredential -> Natural -> ClientId -> NotificationId -> IO (NotificationGap, Notifications))
oneEvent e _ _ _ _ = do
  notifId <- generate arbitrary
  pure
    ( NotificationGapDoesNotExist,
      Notifications False [Notification notifId (Event.KnownEvent e :| [])]
    )

encrypt :: Member (Embed IO) r => CBox.Box -> CBox.Session -> Text -> Sem r Base64ByteString
encrypt box ses msg = do
  let user = UserId "user"
      client = ClientId "client"
  (Recipients (UserClientMap encryptedMap)) <-
    CryptoBoxFFI.run box . assertNoError $
      Message.mkRecipients msg $ UserClientMap $ Map.singleton user $ Map.singleton client ses

  assertLookup client =<< assertLookup user encryptedMap

data TestException = TestException
  deriving (Show, Eq)

instance Exception TestException
