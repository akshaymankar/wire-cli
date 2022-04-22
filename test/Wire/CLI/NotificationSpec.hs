module Wire.CLI.NotificationSpec where

import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Domain
import Data.Id (ClientId, Id (Id), newClientId)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
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
import Wire.API.Message (QualifiedOtrRecipients (QualifiedOtrRecipients))
import Wire.API.User.Client (QualifiedUserClientMap (QualifiedUserClientMap))
import qualified Wire.CLI.App as App
import Wire.CLI.Backend (Backend, ServerCredential)
import Wire.CLI.Backend.Arbitrary (unprocessedNotification)
import Wire.CLI.Backend.Event (Event, ExtensibleEvent (KnownEvent))
import qualified Wire.CLI.Backend.Event as Event
import Wire.CLI.Backend.Notification
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.CryptoBox.TestUtil
import qualified Wire.CLI.Error as WErr
import qualified Wire.CLI.Message as Message
import Wire.CLI.Mocks.Backend
import Wire.CLI.Mocks.CryptoBox (mockGetSessionReturns, mockSaveReturns, mockSessionFromMessageReturns)
import Wire.CLI.Mocks.Store
import Wire.CLI.Notification.Types (ProcessedNotification (DecryptedMessage, PlainNotification))
import qualified Wire.CLI.Notification as Notification
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.CLI.Store.Arbitrary ()
import Wire.CLI.Store.StoredMessage (StoredMessage (StoredMessage))
import Wire.CLI.TestUtil
import qualified Wire.API.Event.Conversation as Conv
import qualified Data.ByteString.Base64 as Base64

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

        processed <- mockMany @MockedEffects . assertNoError $ Notification.sync

        getNotifCalls <- mockGetNotificationsCalls
        embed $ do
          getNotifCalls `shouldBe` [(creds, 1000, client, NotificationId UUID.nil)]
          processed `shouldBe` []

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

        processed <- mockMany @MockedEffects . assertNoError $ Notification.sync

        getNotifCalls <- mockGetNotificationsCalls
        saveNotifIdCalls <- mockSaveLastNotificationIdCalls
        embed $ do
          getNotifCalls
            `shouldBe` [ (creds, 1000, client, prevNotifId),
                         (creds, 1000, client, notificationId lastNotifPage1)
                       ]
          let allNotifs = notifsPage1 <> [lastNotifPage1] <> notifsPage2 <> [lastNotifPage2]
          saveNotifIdCalls
            `shouldBe` map notificationId allNotifs

          processed `shouldBe` concatMap (map PlainNotification . NonEmpty.toList . notificationPayload) allNotifs

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

          void . mockMany @MockedEffects . assertNoError $ Notification.sync

      it "should store new connections" $
        runM . evalMocks @MockedEffects $ do
          mockGetCredsReturns (Just <$> generate arbitrary)
          mockGetClientIdReturns (Just <$> generate arbitrary)
          mockGetLastNotificationIdReturns (pure Nothing)

          conn <- embed $ generate arbitrary
          let connEvent = Event.ConnectionEvent conn Nothing Nothing
          let event = Event.EventUser (Event.EventUserConnection connEvent)
          mockGetNotificationsReturns $ oneEvent event

          processed <- mockMany @MockedEffects . assertNoError $ Notification.sync

          addConnCalls <- mockAddConnectionCalls
          embed $ do
            addConnCalls `shouldBe` [conn]
            processed `shouldBe` [PlainNotification $ KnownEvent event]

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
          encryptedMessage <- Base64.encodeBase64 <$> encrypt aliBox encryptionSession secret
          embed $ print encryptedMessage

          -- Mock 'getNotifications'
          let msg = Conv.OtrMessage aliClient bobClient encryptedMessage Nothing
          sendingTime <- embed Time.getCurrentTime
          let eventData = Conv.EdOtrMessage msg
              event = Event.EventConv (Conv.Event convId ali sendingTime eventData)
          mockGetNotificationsReturns $ oneEvent event

          -- Bob syncs her notifications
          processed <- mockMany @MockedEffects . assertNoError . CryptoBoxFFI.run bobBox $ Notification.sync

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ do
            let sm = StoredMessage ali aliClient sendingTime (Store.ValidMessage secret)
            addMsgCalls `shouldBe` [(convId, sm)]
            processed `shouldBe` [DecryptedMessage convId sm]

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
          keyExchangeMessage <- encrypt bobBox bobSesForAli =<< embed (generate arbitrary)

          -- Ali decrypts the message
          (aliSesForBob, _) <- decryptWithBox aliBox (Message.mkSessionId bob bobClient) keyExchangeMessage

          -- Ali uses same session to encrypt the next message for Bob
          secret <- embed $ generate arbitrary
          encryptedMessage <- Base64.encodeBase64 <$> encrypt aliBox aliSesForBob secret
          let msg = Conv.OtrMessage aliClient bobClient encryptedMessage Nothing
          sendingTime <- embed Time.getCurrentTime

          -- Bob opens the cryptobox again, this simulates new run of the
          -- client, this implies that a session is automatically stored after
          -- its creation.
          bobBox1 <- embed $ App.openCBox bobBoxDir
          processedBobBox1 <-
            mockMany @MockedEffects . CryptoBoxFFI.run bobBox1 $
              Notification.addOtrMessage convId ali sendingTime msg

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ do
            let sm = StoredMessage ali aliClient sendingTime (Store.ValidMessage secret)
            addMsgCalls `shouldBe` [(convId, sm)]
            processedBobBox1 `shouldBe` DecryptedMessage convId sm

          -- Open the box again to make sure previous run saved the session.
          bobBox2 <- embed $ App.openCBox bobBoxDir
          -- Bob recieves a notification again for whatever reason, this is just
          -- a round about way of making sure the session was previously saved.
          -- This shouldn't happen in the wild.
          processedBobBox2 <-
            mockMany @MockedEffects . CryptoBoxFFI.run bobBox2 $
              Notification.addOtrMessage convId ali sendingTime msg

          addMsgCallsAfterDuplicate <- mockAddMessageCalls
          embed $ do
            let sm = StoredMessage ali aliClient sendingTime (Store.InvalidMessage "failed to decrypt: DuplicateMessage")
            tail addMsgCallsAfterDuplicate `shouldBe` [(convId, sm)]
            processedBobBox2 `shouldBe` DecryptedMessage convId sm

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
          keyExchangeMessage <- encrypt bobBox bobSesForAli =<< embed (generate arbitrary)

          -- Ali decrypts the message
          (aliSesForBob, _) <- decryptWithBox aliBox (Message.mkSessionId bob bobClient) keyExchangeMessage

          -- Ali uses same session to encrypt the next message for Bob
          secret <- embed $ generate arbitrary
          encryptedMessage <- Base64.encodeBase64 <$> encrypt aliBox aliSesForBob secret
          let msg = Conv.OtrMessage aliClient bobClient encryptedMessage Nothing
          sendingTime <- embed Time.getCurrentTime

          let failedAttempt = runM . evalMocks @MockedEffects $ do
                -- Mock store to throw error
                mockAddMessageReturns (\_ _ -> throwIO TestException)
                -- Bob syncs her notification
                mockMany @MockedEffects . CryptoBoxFFI.run bobBox $
                  Notification.addOtrMessage convId ali sendingTime msg

          embed $ failedAttempt `shouldThrow` (== TestException)

          -- Bob opens the cryptobox again, this simulates the client crashing.
          -- We do not catch any failures in saving, so this is correct.
          bobBoxReopened <- embed $ App.openCBox bobBoxDir
          -- Should succeed this time
          processed <-
            mockMany @MockedEffects . CryptoBoxFFI.run bobBoxReopened $
              Notification.addOtrMessage convId ali sendingTime msg

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ do
            let sm = StoredMessage ali aliClient sendingTime (Store.ValidMessage secret)
            addMsgCalls `shouldBe` [(convId, sm)]
            processed `shouldBe` DecryptedMessage convId sm

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
          encryptedSecretFail <- fmap Base64.encodeBase64 . embed $ generate arbitrary
          let msgFail = Conv.OtrMessage aliClient bobClient encryptedSecretFail Nothing
          sendingTimeFail <- embed Time.getCurrentTime
          let eventDataFail = Conv.EdOtrMessage msgFail
              eventFail = Event.EventConv (Conv.Event convId ali sendingTimeFail eventDataFail)

          -- A message wich succeds to decrypt
          secretSuccess <- embed $ generate arbitrary
          encryptedSecret <- fmap Base64.encodeBase64 . embed $ generate arbitrary
          let msgSuccess = Conv.OtrMessage aliClient bobClient encryptedSecret Nothing
          sendingTimeSuccess <- embed Time.getCurrentTime
          let eventDataSuccess = Conv.EdOtrMessage msgSuccess
              eventSuccess = Event.EventConv (Conv.Event convId ali sendingTimeSuccess eventDataSuccess)

          mockGetNotificationsReturns $ manyEvents (eventFail :| [eventSuccess])
          mockGetSessionReturns (const $ pure CBox.NoSession)
          ses <- randomSession
          mockSessionFromMessageReturns $ \_ enc ->
            if Base64.encodeBase64 enc == encryptedSecretFail
              then pure CBox.InvalidMessage
              else pure $ CBox.Success (ses, Proto.encodeMessage secretSuccess)
          mockSaveReturns (const . pure $ CBox.Success ())

          -- Bob syncs her notifications
          processed <- mockMany @MockedEffects . assertNoError $ Notification.sync

          -- Bob should have the decrypted message in her 'Store'
          addMsgCalls <- mockAddMessageCalls
          embed $ do
            let smFail = StoredMessage ali aliClient sendingTimeFail (Store.InvalidMessage "failed to decrypt: InvalidMessage")
                smSuccess = StoredMessage ali aliClient sendingTimeSuccess (Store.ValidMessage secretSuccess)
            addMsgCalls
              `shouldBe` [ (convId, smFail),
                           (convId, smSuccess)
                         ]
            processed
              `shouldBe` [ DecryptedMessage convId smFail,
                           DecryptedMessage convId smSuccess
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

encrypt :: Member (Embed IO) r => CBox.Box -> CBox.Session -> GenericMessage -> Sem r ByteString
encrypt box ses msg = do
  let domain = Domain "foo.example.com"
      user = Id UUID.nil
      client = newClientId 1234
  (QualifiedOtrRecipients (QualifiedUserClientMap encryptedMap)) <-
    CryptoBoxFFI.run box . assertNoError $
      Message.mkRecipients msg $
        QualifiedUserClientMap
          . Map.singleton domain
          . Map.singleton user
          $ Map.singleton client ses

  assertLookup3 domain user client encryptedMap

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

expectationFailure' :: (HasCallStack, MonadIO m) => String -> m a
expectationFailure' err = do
  liftIO $ expectationFailure err
  error "Impossible"

data TestException = TestException
  deriving (Show, Eq)

instance Exception TestException
