module Wire.CLI.MessageSpec where

import Data.ByteString.Base64 (decodeBase64Lenient)
import Data.Domain
import Data.Id (Id (Id), newClientId)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Polysemy
import qualified Polysemy.Error as Error
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.API.Message (QualifiedOtrRecipients (QualifiedOtrRecipients))
import Wire.API.User.Client (QualifiedUserClientMap (QualifiedUserClientMap))
import Wire.API.User.Client.Prekey (Prekey (prekeyKey))
import Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.CryptoBox.TestUtil
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Message (getOrCreateSession, mkRecipients, mkSessionId)
import qualified Wire.CLI.Mocks.CryptoBox as CryptoBox
import qualified Wire.CLI.Store as Store
import Wire.CLI.Store.Arbitrary ()
import Wire.CLI.TestUtil

type MockedEffects = '[CryptoBox]

spec :: Spec
spec = describe "Message" $ do
  describe "mkSessionId" $ do
    it "should be made by concatenating userId and clientId" $ do
      let user = Qualified (Id UUID.nil) (Domain "example.com")
          client = newClientId 1234
          CBox.SID actualSid = mkSessionId user client
      actualSid `shouldBe` "example.com_00000000-0000-0000-0000-000000000000_4d2"

  describe "getOrCreateSession" $ do
    -- Happy path cannot be tested because cryptobox-haskell doesn't export
    -- constructor for 'Session'
    it "should error if getting a session fails in an unknown way" $
      runM . evalMocks @MockedEffects $ do
        (domain, userId, clientId) <- embed $ generate arbitrary
        prekey <- generateArbitraryPrekey
        cboxErr <- embed $ generate $ anyFailureExcept [CBox.NoSession]
        CryptoBox.mockGetSessionReturns (\_ -> pure $ castCBoxError cboxErr)
        eitherErr <- Error.runError $ mockMany @MockedEffects $ getOrCreateSession (domain, userId, clientId) prekey

        getCalls <- CryptoBox.mockGetSessionCalls
        embed $ getCalls `shouldBe` [mkSessionId (Qualified userId domain) clientId]

        embed $ case eitherErr of
          Left (WireCLIError.UnexpectedCryptoBoxError actualCboxErr) -> actualCboxErr `shouldBe` cboxErr
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got session"

    it "should error if creating a session fails" $
      runM . evalMocks @MockedEffects $ do
        (domain, userId, clientId) <- embed $ generate arbitrary
        prekey <- generateArbitraryPrekey
        CryptoBox.mockGetSessionReturns (\_ -> pure CBox.NoSession)

        cboxErr <- embed $ generate $ anyFailureExcept []
        CryptoBox.mockSessionFromPrekeyReturns (\_ _ -> pure $ castCBoxError cboxErr)

        eitherErr <- Error.runError $ mockMany @MockedEffects $ getOrCreateSession (domain, userId, clientId) prekey

        getCalls <- CryptoBox.mockGetSessionCalls
        embed $ getCalls `shouldBe` [mkSessionId (Qualified userId domain) clientId]

        createCalls <- CryptoBox.mockSessionFromPrekeyCalls
        embed $ createCalls `shouldBe` [(mkSessionId (Qualified userId domain) clientId, decodeBase64Lenient . Text.encodeUtf8 $ prekeyKey prekey)]

        embed $ case eitherErr of
          Left (WireCLIError.UnexpectedCryptoBoxError actualCboxErr) -> actualCboxErr `shouldBe` cboxErr
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got session"

  describe "mkRecipients" $ do
    it "should encrypt a message for given recipients" $ do
      (domain1, u1, c11, c12) <- generate arbitrary
      (domain2, u2, c21) <- generate arbitrary

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
            QualifiedUserClientMap $
              Map.fromList
                [ (domain1, Map.singleton u1 (Map.fromList [(c11, ses11), (c12, ses12)])),
                  (domain2, Map.singleton u2 (Map.fromList [(c21, ses21)]))
                ]

      secret <- generate arbitrary
      (QualifiedOtrRecipients (QualifiedUserClientMap recipients)) <-
        runM . CryptoBoxFFI.run senderBox . assertNoError $
          mkRecipients secret sessionMap

      enc11 <- assertLookup3 domain1 u1 c11 recipients
      enc12 <- assertLookup3 domain1 u1 c12 recipients
      enc21 <- assertLookup3 domain2 u2 c21 recipients

      -- Ensure that the messages are not encrypted for anyone else
      Map.size recipients `shouldBe` 2
      assertLookup2 domain1 u1 recipients >>= \m -> Map.size m `shouldBe` 2
      assertLookup2 domain2 u2 recipients >>= \m -> Map.size m `shouldBe` 1

      (_, msg11) <- runM $ decryptWithBox box11 (CBox.SID "ses") enc11
      (_, msg12) <- runM $ decryptWithBox box12 (CBox.SID "ses") enc12
      (_, msg21) <- runM $ decryptWithBox box21 (CBox.SID "ses") enc21

      Store.decodeMessage msg11 `shouldBe` Store.ValidMessage secret
      Store.decodeMessage msg12 `shouldBe` Store.ValidMessage secret
      Store.decodeMessage msg21 `shouldBe` Store.ValidMessage secret

    it "should fail if encryption fails" $
      runM . evalMocks @MockedEffects $ do
        (receiverDomain, receiverUser, receiverClient) <- embed $ generate arbitrary
        receiverBox <- getTempCBox
        pk <- newPrekeyWithBox receiverBox 0x3789

        senderBox <- getTempCBox
        ses <- sessionWithBox senderBox (CBox.SID "ses") pk

        cboxErr <- embed $ generate $ anyFailureExcept [CBox.NoSession]
        CryptoBox.mockEncryptReturns (\_ _ -> pure $ castCBoxError cboxErr)

        secret <- embed $ generate arbitrary
        let sessionMap =
              QualifiedUserClientMap
                . Map.singleton receiverDomain
                . Map.singleton receiverUser
                $ Map.singleton receiverClient ses
        eitherErr <- Error.runError . mockMany @MockedEffects $ mkRecipients secret sessionMap

        embed $ case eitherErr of
          Left (WireCLIError.UnexpectedCryptoBoxError actualCboxErr) -> actualCboxErr `shouldBe` cboxErr
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got recipients"
