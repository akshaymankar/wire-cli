module Wire.CLI.NotificationSpec where

import Control.Exception (Exception, throwIO)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Numeric.Natural (Natural)
import Polysemy
import qualified Polysemy.Error as Error
import Proto.Messages (GenericMessage)
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
import Wire.CLI.Mocks.CryptoBox (mockGetSessionReturns, mockSaveReturns, mockSessionFromMessageReturns)
import Wire.CLI.Mocks.Store
import qualified Wire.CLI.Notification as Notification
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.CLI.Store.Arbitrary ()
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

        embed $ case eitherErr of
          Left (WErr.ErrorInvalidState invalidState) -> invalidState `shouldBe` WErr.NoClientFound
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got session"

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

          mockGetNotificationsReturns
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
          bobBox <- getTempCBox
          bobKey <- newPrekeyWithBox bobBox 0x1231
          aliBox <- getTempCBox

          -- Ali encrypts a message for Bob
          encryptionSession <- sessionWithBox aliBox (CBox.SID "ses") bobKey
          secret <- embed $ generate arbitrary
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
          embed $ addMsgCalls `shouldBe` [(convId, StoredMessage ali aliClient sendingTime (Store.ValidMessage secret))]

      it "should save the session after saving the message" $ do
        runM . evalMocks @MockedEffects $ do
          -- Users and conversations
          (ali, aliClient) <- embed $ generate arbitrary
          (bob, bobClient) <- embed $ generate arbitrary
          convId <- embed $ generate arbitrary

          -- Initialized the cryptoboxes
          bobBoxDir <- getTempCBoxDir
          bobBox <- embed $ App.openCBox bobBoxDir
          _ <- newPrekeyWithBox bobBox 0x1231
          aliBox <- getTempCBox
          aliKey <- newPrekeyWithBox aliBox 0x7373

          -- Bob sends the first message to Ali
          bobSesForAli <- sessionWithBox bobBox (Message.mkSessionId ali aliClient) aliKey
          (Base64ByteString keyExchangeMessage) <- encrypt bobBox bobSesForAli =<< embed (generate arbitrary)

          -- Ali decrypts the message
          (aliSesForBob, _) <- decryptWithBox aliBox (Message.mkSessionId bob bobClient) keyExchangeMessage

          -- Ali uses same session to encrypt the next message for Bob
          secret <- embed $ generate arbitrary
          encryptedMessage <- encrypt aliBox aliSesForBob secret
          let msg = Event.OtrMessage aliClient bobClient encryptedMessage Nothing
          sendingTime <- embed Time.getCurrentTime

          -- Bob opens the cryptobox again, this simulates new run of the
          -- client, this implies that a session is automatically stored after
          -- its creation.
          bobBox1 <- embed $ App.openCBox bobBoxDir
          mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBox1 $
            Notification.addOtrMessage convId ali sendingTime msg

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ addMsgCalls `shouldBe` [(convId, StoredMessage ali aliClient sendingTime (Store.ValidMessage secret))]

          -- Open the box again to make sure previous run saved the session.
          bobBox2 <- embed $ App.openCBox bobBoxDir
          -- Bob recieves a notification again for whatever reason, this is just
          -- a round about way of making sure the session was previously saved.
          -- This shouldn't happen in the wild.
          mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBox2 $
            Notification.addOtrMessage convId ali sendingTime msg

          addMsgCallsAfterDuplicate <- mockAddMessageCalls
          embed $ tail addMsgCallsAfterDuplicate `shouldBe` [(convId, StoredMessage ali aliClient sendingTime (Store.InvalidMessage "failed to decrypt: DuplicateMessage"))]

      it "should be able to decrypt a message again if saving message fails" $ do
        runM . evalMocks @MockedEffects $ do
          -- Users and conversations
          (ali, aliClient) <- embed $ generate arbitrary
          (bob, bobClient) <- embed $ generate arbitrary
          convId <- embed $ generate arbitrary

          -- Initialized the cryptoboxes
          bobBoxDir <- getTempCBoxDir
          bobBox <- embed $ App.openCBox bobBoxDir
          _ <- newPrekeyWithBox bobBox 0x1231
          aliBox <- getTempCBox
          aliKey <- newPrekeyWithBox aliBox 0x7373

          -- Bob sends the first message to Ali
          bobSesForAli <- sessionWithBox bobBox (Message.mkSessionId ali aliClient) aliKey
          (Base64ByteString keyExchangeMessage) <- encrypt bobBox bobSesForAli =<< embed (generate arbitrary)

          -- Ali decrypts the message
          (aliSesForBob, _) <- decryptWithBox aliBox (Message.mkSessionId bob bobClient) keyExchangeMessage

          -- Ali uses same session to encrypt the next message for Bob
          secret <- embed $ generate arbitrary
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

          -- Bob opens the cryptobox again, this simulates the client crashing.
          -- We do not catch any failures in saving, so this is correct.
          bobBoxReopened <- embed $ App.openCBox bobBoxDir
          -- Should succeed this time
          mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBoxReopened $
            Notification.addOtrMessage convId ali sendingTime msg

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ addMsgCalls `shouldBe` [(convId, StoredMessage ali aliClient sendingTime (Store.ValidMessage secret))]

      it "should store failed decryption in case decryption fails and continue decrypting" $
        runM . evalMocks @MockedEffects $ do
          mockGetCredsReturns (Just <$> generate arbitrary)
          mockGetClientIdReturns (Just <$> generate arbitrary)
          mockGetLastNotificationIdReturns (pure Nothing)

          -- Users, clients and conv
          (ali, aliClient) <- embed $ generate arbitrary
          bobClient <- embed $ generate arbitrary
          convId <- embed $ generate arbitrary

          -- A message wich fails to decrypt
          encryptedSecretFail <- embed $ generate arbitrary
          let msgFail = Event.OtrMessage aliClient bobClient encryptedSecretFail Nothing
          sendingTimeFail <- embed Time.getCurrentTime
          let eventDataFail = Event.EventConvOtrMessageAdd msgFail
              eventFail = Event.EventConv (Event.ConvEvent convId ali sendingTimeFail eventDataFail)

          -- A message wich succeds to decrypt
          secretSuccess <- embed $ generate arbitrary
          encryptedSecret <- embed $ generate arbitrary
          let msgSuccess = Event.OtrMessage aliClient bobClient encryptedSecret Nothing
          sendingTimeSuccess <- embed Time.getCurrentTime
          let eventDataSuccess = Event.EventConvOtrMessageAdd msgSuccess
              eventSuccess = Event.EventConv (Event.ConvEvent convId ali sendingTimeSuccess eventDataSuccess)

          mockGetNotificationsReturns $ manyEvents (eventFail :| [eventSuccess])
          mockGetSessionReturns (const $ pure CBox.NoSession)
          ses <- randomSession
          mockSessionFromMessageReturns $ \_ enc ->
            if Base64ByteString enc == encryptedSecretFail
              then pure CBox.InvalidMessage
              else pure $ CBox.Success (ses, Proto.encodeMessage secretSuccess)
          mockSaveReturns (const . pure $ CBox.Success ())

          -- Bob syncs her notifications
          mockMany @MockedEffects . assertNoError $ Notification.sync

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $
            addMsgCalls
              `shouldBe` [ (convId, StoredMessage ali aliClient sendingTimeFail (Store.InvalidMessage "failed to decrypt: InvalidMessage")),
                           (convId, StoredMessage ali aliClient sendingTimeSuccess (Store.ValidMessage secretSuccess))
                         ]

oneEvent ::
  Event ->
  (ServerCredential -> Natural -> ClientId -> NotificationId -> IO (NotificationGap, Notifications))
oneEvent e _ _ _ _ = do
  notifId <- generate arbitrary
  pure
    ( NotificationGapDoesNotExist,
      Notifications False [Notification notifId (Event.KnownEvent e :| [])]
    )

manyEvents :: NonEmpty Event -> (ServerCredential -> Natural -> ClientId -> NotificationId -> IO (NotificationGap, Notifications))
manyEvents e _ _ _ _ = do
  notifId <- generate arbitrary
  pure
    ( NotificationGapDoesNotExist,
      Notifications False [Notification notifId (Event.KnownEvent <$> e)]
    )

encrypt :: Member (Embed IO) r => CBox.Box -> CBox.Session -> GenericMessage -> Sem r Base64ByteString
encrypt box ses msg = do
  let user = UserId "user"
      client = ClientId "client"
  (Recipients (UserClientMap encryptedMap)) <-
    CryptoBoxFFI.run box . assertNoError $
      Message.mkRecipients msg $ UserClientMap $ Map.singleton user $ Map.singleton client ses

  assertLookup client =<< assertLookup user encryptedMap

randomSession :: Member (Embed IO) r => Sem r CBox.Session
randomSession = do
  (ali, aliClient) <- embed $ generate arbitrary
  bobBoxDir <- getTempCBoxDir
  bobBox <- embed $ App.openCBox bobBoxDir
  _ <- newPrekeyWithBox bobBox 0x1231
  aliBox <- getTempCBox
  aliKey <- newPrekeyWithBox aliBox 0x7373

  -- Bob sends the first message to Ali
  sessionWithBox bobBox (Message.mkSessionId ali aliClient) aliKey

data TestException = TestException
  deriving (Show, Eq)

instance Exception TestException
