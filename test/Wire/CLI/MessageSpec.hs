module Wire.CLI.MessageSpec where

import qualified Data.Map as Map
import Polysemy
import qualified Polysemy.Error as Error
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.Backend.Client (ClientId (ClientId))
import Wire.CLI.Backend.Message (Recipients (Recipients), UserClientMap (UserClientMap))
import Wire.CLI.Backend.User (UserId (UserId))
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.CryptoBox.TestUtil
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Message (getOrCreateSession, mkRecipients, mkSessionId)
import qualified Wire.CLI.Mocks.CryptoBox as CryptoBox
import qualified Wire.CLI.Store as Store
import Wire.CLI.Store.Arbitrary ()
import Wire.CLI.TestUtil
import Wire.CLI.Util.ByteStringJSON (Base64ByteString (Base64ByteString))

type MockedEffects = '[CryptoBox]

spec :: Spec
spec = describe "Message" $ do
  describe "mkSessionId" $ do
    it "should be made by concatenating userId and clientId" $ do
      mkSessionId (UserId "user") (ClientId "client") `shouldBe` CBox.SID "user_client"
  describe "getOrCreateSession" $ do
    -- Happy path cannot be tested because cryptobox-haskell doesn't export
    -- constructor for 'Session'
    it "should error if getting a session fails in an unknown way" $
      runM . evalMocks @MockedEffects $ do
        (userId, clientId, prekey) <- embed $ generate arbitrary
        cboxErr <- embed $ generate $ anyFailureExcept [CBox.NoSession]
        CryptoBox.mockGetSessionReturns (\_ -> pure $ castCBoxError cboxErr)
        eitherErr <- Error.runError $ mockMany @MockedEffects $ getOrCreateSession (userId, clientId) prekey

        getCalls <- CryptoBox.mockGetSessionCalls
        embed $ getCalls `shouldBe` [mkSessionId userId clientId]

        embed $ case eitherErr of
          Left (WireCLIError.UnexpectedCryptoBoxError actualCboxErr)-> actualCboxErr `shouldBe` cboxErr
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got session"

    it "should error if creating a session fails" $
      runM . evalMocks @MockedEffects $ do
        (userId, clientId, prekey) <- embed $ generate arbitrary
        CryptoBox.mockGetSessionReturns (\_ -> pure CBox.NoSession)

        cboxErr <- embed $ generate $ anyFailureExcept []
        CryptoBox.mockSessionFromPrekeyReturns (\_ _ -> pure $ castCBoxError cboxErr)

        eitherErr <- Error.runError $ mockMany @MockedEffects $ getOrCreateSession (userId, clientId) prekey

        getCalls <- CryptoBox.mockGetSessionCalls
        embed $ getCalls `shouldBe` [mkSessionId userId clientId]

        createCalls <- CryptoBox.mockSessionFromPrekeyCalls
        embed $ createCalls `shouldBe` [(mkSessionId userId clientId, prekey)]

        embed $ case eitherErr of
          Left (WireCLIError.UnexpectedCryptoBoxError actualCboxErr) -> actualCboxErr `shouldBe` cboxErr
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got session"

  describe "mkRecipients" $ do
    it "should encrypt a message for given recipients" $ do
      (u1, c11, c12) <- generate arbitrary
      (u2, c21) <- generate arbitrary

      box11 <- runM getTempCBox
      box12 <- runM getTempCBox
      box21 <- runM getTempCBox

      pk11 <- runM $ newPrekeyWithBox box11 0x948d
      pk12 <- runM $ newPrekeyWithBox box12 0x63d9
      pk21 <- runM $ newPrekeyWithBox box21 0xae01

      senderBox <- runM getTempCBox
      ses11 <- runM $ sessionWithBox senderBox (CBox.SID "ses11") pk11
      ses12 <- runM $ sessionWithBox senderBox (CBox.SID "ses12") pk12
      ses21 <- runM $ sessionWithBox senderBox (CBox.SID "ses21") pk21
      let sessionMap =
            UserClientMap $
              Map.fromList
                [ (u1, Map.fromList [(c11, ses11), (c12, ses12)]),
                  (u2, Map.fromList [(c21, ses21)])
                ]

      secret <- generate arbitrary
      (Recipients (UserClientMap recipients)) <-
        runM . CryptoBoxFFI.run senderBox . assertNoError $
          mkRecipients secret sessionMap

      (Base64ByteString enc11) <- assertLookup c11 =<< assertLookup u1 recipients
      (Base64ByteString enc12) <- assertLookup c12 =<< assertLookup u1 recipients
      (Base64ByteString enc21) <- assertLookup c21 =<< assertLookup u2 recipients

      -- Ensure that the messages are not encrypted for anyone else
      Map.size recipients `shouldBe` 2
      assertLookup u1 recipients >>= \m -> Map.size m `shouldBe` 2
      assertLookup u2 recipients >>= \m -> Map.size m `shouldBe` 1

      (_, msg11) <- runM $ decryptWithBox box11 (CBox.SID "ses") enc11
      (_, msg12) <- runM $ decryptWithBox box12 (CBox.SID "ses") enc12
      (_, msg21) <- runM $ decryptWithBox box21 (CBox.SID "ses") enc21

      Store.decodeMessage msg11 `shouldBe` Store.ValidMessage secret
      Store.decodeMessage msg12 `shouldBe` Store.ValidMessage secret
      Store.decodeMessage msg21 `shouldBe` Store.ValidMessage secret

    it "should fail if encryption fails" $
      runM . evalMocks @MockedEffects $ do
        (receiverUser, receiverClient) <- embed $ generate arbitrary
        receiverBox <- getTempCBox
        pk <- newPrekeyWithBox receiverBox 0x3789

        senderBox <- getTempCBox
        ses <- sessionWithBox senderBox (CBox.SID "ses") pk

        cboxErr <- embed $ generate $ anyFailureExcept [CBox.NoSession]
        CryptoBox.mockEncryptReturns (\_ _ -> pure $ castCBoxError cboxErr)

        secret <- embed $ generate arbitrary
        eitherErr <- Error.runError . mockMany @MockedEffects $ mkRecipients secret $ UserClientMap $ Map.singleton receiverUser $ Map.singleton receiverClient ses

        embed $ case eitherErr of
          Left (WireCLIError.UnexpectedCryptoBoxError actualCboxErr) -> actualCboxErr `shouldBe` cboxErr
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got recipients"
