{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.CLI.ExecuteSpec where

import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Lens.Family2 ((&), (.~))
import qualified Network.URI as URI
import Polysemy
import qualified Polysemy.Error as Error
import qualified Polysemy.Random as Random
import qualified Proto.Messages as M
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Arbitrary (unprocessedNotification)
import qualified Wire.CLI.Backend.Client as Client
import Wire.CLI.Backend.CommonTypes (Name (..))
import qualified Wire.CLI.Backend.Message as Backend
import Wire.CLI.Backend.User (Email (..))
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import Wire.CLI.CryptoBox.TestUtil
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Execute as Execute
import Wire.CLI.Mocks.Backend as Backend
import Wire.CLI.Mocks.CryptoBox as CryptoBox
import Wire.CLI.Mocks.Store as Store
import Wire.CLI.Mocks.UUIDGen ()
import qualified Wire.CLI.Mocks.UUIDGen as UUIDGen
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import Wire.CLI.Store.Arbitrary ()
import Wire.CLI.TestUtil
import Wire.CLI.UUIDGen (UUIDGen)
import Wire.CLI.Util.ByteStringJSON (Base64ByteString (..))

type MockedEffects = '[Backend, Store, CryptoBox, UUIDGen]

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
{-# ANN spec ("HLint: ignore Reduce duplication" :: String) #-}
spec :: Spec
spec = do
  describe "Execute Login" $ do
    let Just server = URI.parseURI "https://be.example.com"
    let loginOpts = Opts.LoginOptions server (Opts.LoginHandle "handle") "pwwpw"
    let loginCommand = Opts.Login loginOpts
    it "should login and store creds" $
      runM . evalMocks @MockedEffects $ do
        cred <- embed $ generate arbitrary
        prekey <- embed $ generate arbitrary
        mockLoginReturns (const $ pure $ Backend.LoginSuccess cred)
        mockNewPrekeyReturns (const . pure $ CBox.Success prekey)
        clientId <- embed $ Client.ClientId <$> generate arbitrary
        mockRegisterClientReturns
          ( \_ Client.NewClient {..} ->
              pure
                Client.Client
                  { clientId = clientId,
                    clientCookie = newClientCookie,
                    clientModel = newClientModel,
                    clientType = newClientType,
                    clientClass = newClientClass,
                    clientLabel = newClientLabel
                  }
          )
        -- No previous client
        mockGetClientIdReturns (pure Nothing)

        -- execute the command
        maybeLoginErr <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute loginCommand

        -- Expectations:
        -- No error
        embed $ maybeLoginErr `shouldBe` Nothing
        -- Call backend
        loginCalls' <- mockLoginCalls
        embed $ loginCalls' `shouldBe` [loginOpts]
        -- Store creds
        saveCredsCalls' <- mockSaveCredsCalls
        embed $ saveCredsCalls' `shouldBe` [Backend.ServerCredential server cred]
        -- Register Client
        assertGenKeysAndRegisterClient (Backend.ServerCredential server cred) (Just $ Opts.loginPassword loginOpts)

    it "should not create a client if one exists" $
      runM . evalMocks @MockedEffects $ do
        cred <- embed $ generate arbitrary
        clientId <- embed $ Client.ClientId <$> generate arbitrary
        mockLoginReturns (const $ pure $ Backend.LoginSuccess cred)
        mockGetClientIdReturns (pure $ Just clientId)

        -- execute the command
        maybeLoginErr <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute loginCommand

        -- Expectations:
        -- No error
        embed $ maybeLoginErr `shouldBe` Nothing

        -- Call backend
        loginCalls' <- mockLoginCalls
        embed $ loginCalls' `shouldBe` [loginOpts]
        -- Store creds
        saveCredsCalls' <- mockSaveCredsCalls
        embed $ saveCredsCalls' `shouldBe` [Backend.ServerCredential server cred]

        -- No Register Client
        registerClientCalls' <- mockRegisterClientCalls
        embed $ length registerClientCalls' `shouldBe` 0

    it "should error when login fails" $
      runM . evalMocks @MockedEffects $ do
        mockLoginReturns (const $ pure $ Backend.LoginFailure "something failed")

        maybeLoginErr <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute loginCommand

        embed $ maybeLoginErr `shouldBe` Just "something failed"

  describe "Execute RefreshToken" $ do
    it "should refresh token and save new creds" $
      runM . evalMocks @MockedEffects $ do
        oldCred <- embed $ generate arbitrary
        let server = Backend.server oldCred
        newCred <- embed $ generate arbitrary
        mockGetCredsReturns (pure $ Just oldCred)
        mockRefreshTokenReturns (\_ _ -> pure newCred)

        mockMany @MockedEffects . assertNoError . assertNoRandomness $ Execute.execute Opts.RefreshToken

        refreshCalls <- mockRefreshTokenCalls
        embed $ refreshCalls `shouldBe` [(server, Backend.credentialCookies . Backend.credential $ oldCred)]

        saveCalls <- mockSaveCredsCalls
        embed $ saveCalls `shouldBe` [Backend.ServerCredential server newCred]

    it "should error when there are no creds" $
      runM . evalMocks @MockedEffects $ do
        mockGetCredsReturns (pure Nothing)

        eitherErr <-
          mockMany @MockedEffects . Error.runError . assertNoRandomness $
            Execute.execute Opts.RefreshToken

        embed $ case eitherErr of
          Left WireCLIError.NotLoggedIn -> pure ()
          Left unexpectedErr -> expectationFailure $ "Unexpected error: " <> show unexpectedErr
          Right _ -> expectationFailure "Expected error, got none"

  describe "Execute SyncConvs" $ do
    it "should get convs from the server and store them" $
      runM . evalMocks @MockedEffects $ do
        convs <- embed $ generate arbitrary
        creds <- embed $ generate arbitrary
        mockGetCredsReturns $ pure (Just creds)
        mockListConvsReturns $ \_ _ _ -> do
          pure $ Backend.Convs convs False

        mockMany @MockedEffects . assertNoError . assertNoRandomness $
          Execute.execute Opts.SyncConvs

        saveConvs <- mockSaveConvsCalls
        embed $ saveConvs `shouldBe` [convs]

  describe "Execute ListConvs" $ do
    it "should list convs from the store" $
      runM . evalMocks @MockedEffects $ do
        convs <- embed $ generate arbitrary
        mockGetConvsReturns $ pure (Just convs)

        actualConvs <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute Opts.ListConvs

        embed $ actualConvs `shouldBe` convs

  describe "Execute SyncNotification" $ do
    it "should get notifications and store the last one" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        mockGetCredsReturns $ pure (Just creds)
        lastNotification <- embed $ generate unprocessedNotification
        allButLastNotification <- embed $ generate $ listOf unprocessedNotification
        let notifications = Backend.Notifications False (allButLastNotification <> [lastNotification])
        previousLastNotificationId <- embed $ generate arbitrary
        clientId <- embed $ generate arbitrary

        mockGetNotificationsReturns $ \_ _ _ _ -> pure (Backend.NotificationGapExists, notifications)
        mockGetLastNotificationIdReturns $ pure (Just previousLastNotificationId)
        mockGetClientIdReturns $ pure (Just clientId)

        mockMany @MockedEffects . assertNoError . assertNoRandomness $
          Execute.execute Opts.SyncNotifications

        notifCalls <- mockGetNotificationsCalls
        embed $ notifCalls `shouldBe` [(creds, 1000, clientId, previousLastNotificationId)]
        saveNotifCalls <- mockSaveLastNotificationIdCalls
        embed $ saveNotifCalls `shouldBe` [Backend.notificationId lastNotification]

  describe "Execute RegisterWireless" $ do
    it "should register, store the credential and register a client" $
      runM . evalMocks @MockedEffects $ do
        let Just server = URI.parseURI "https://be.example.com"
        let registerOpts = Opts.RegisterWirelessOptions server (Name "wireless-user")
        cookies <- embed $ generate arbitrary
        newCreds <- embed $ generate arbitrary
        clientId <- embed $ Client.ClientId <$> generate arbitrary
        prekey <- embed $ generate arbitrary

        mockRegisterWirelessReturns $ \_ -> pure cookies
        mockRefreshTokenReturns $ \_ _ -> pure newCreds
        mockNewPrekeyReturns (const . pure $ CBox.Success prekey)
        mockRegisterClientReturns
          ( \_ Client.NewClient {..} ->
              pure
                Client.Client
                  { clientId = clientId,
                    clientCookie = newClientCookie,
                    clientModel = newClientModel,
                    clientType = newClientType,
                    clientClass = newClientClass,
                    clientLabel = newClientLabel
                  }
          )

        -- TODO: Figure out how to mock random
        mockMany @MockedEffects . assertNoError . Random.runRandomIO $
          Execute.execute (Opts.RegisterWireless registerOpts)

        -- Register and get token
        regCalls <- mockRegisterWirelessCalls
        embed $ regCalls `shouldBe` [registerOpts]
        refreshCalls <- mockRefreshTokenCalls
        embed $ refreshCalls `shouldBe` [(server, cookies)]
        -- Save the creds
        let expectedCred = Backend.ServerCredential server newCreds
        saveCredsCalls' <- mockSaveCredsCalls
        embed $ saveCredsCalls' `shouldBe` [expectedCred]
        -- Register a client
        assertGenKeysAndRegisterClient expectedCred Nothing

  describe "Execute Search" $ do
    it "should display the search results" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        results <- embed $ generate arbitrary
        mockGetCredsReturns (pure $ Just creds)
        mockSearchReturns (\_ _ -> pure results)

        let searchOpts = Opts.SearchOptions "query" 10
        returnedRes <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute (Opts.Search searchOpts)

        embed $ returnedRes `shouldBe` results

    it "should error when user is not logged in" $
      runM . evalMocks @MockedEffects $ do
        let searchOpts = Opts.SearchOptions "query" 10
        assertNoUnauthenticatedAccess . mockMany @MockedEffects . assertNoRandomness $
          Execute.execute (Opts.Search searchOpts)

  describe "Execute RequestActivationCode" $ do
    it "should request an activate code from the backend" $
      runM . evalMocks @MockedEffects $ do
        let Just server = URI.parseURI "https://be.example.com"
        let opts = Opts.RequestActivationCodeOptions server (Email "foo@example.com") "en-US"

        mockMany @MockedEffects . assertNoError . assertNoRandomness $
          Execute.execute (Opts.RequestActivationCode opts)

        activationRequests <- mockRequestActivationCodeCalls
        embed $ activationRequests `shouldBe` [opts]

  describe "Execute Register" $ do
    it "should register and create a client" $
      runM . evalMocks @MockedEffects $ do
        let Just server = URI.parseURI "https://be.example.com"
        let registerOpts = Opts.RegisterOptions server (Name "wired-user") (Email "wired@example.com") "123444" Nothing
        cookies <- embed $ generate arbitrary
        newCreds <- embed $ generate arbitrary
        clientId <- embed $ Client.ClientId <$> generate arbitrary
        prekey <- embed $ generate arbitrary

        mockRegisterReturns $ \_ -> pure cookies
        mockRefreshTokenReturns $ \_ _ -> pure newCreds
        mockNewPrekeyReturns (const . pure $ CBox.Success prekey)
        mockRegisterClientReturns
          ( \_ Client.NewClient {..} ->
              pure
                Client.Client
                  { clientId = clientId,
                    clientCookie = newClientCookie,
                    clientModel = newClientModel,
                    clientType = newClientType,
                    clientClass = newClientClass,
                    clientLabel = newClientLabel
                  }
          )

        -- TODO: Figure out how to mock random
        mockMany @MockedEffects . assertNoError . Random.runRandomIO $
          Execute.execute (Opts.Register registerOpts)

        -- Register and get token
        regCalls <- mockRegisterCalls
        embed $ regCalls `shouldBe` [registerOpts]
        refreshCalls <- mockRefreshTokenCalls
        embed $ refreshCalls `shouldBe` [(server, cookies)]
        -- Save the creds
        let expectedCred = Backend.ServerCredential server newCreds
        saveCredsCalls' <- mockSaveCredsCalls
        embed $ saveCredsCalls' `shouldBe` [expectedCred]
        -- Register a client
        assertGenKeysAndRegisterClient expectedCred Nothing

  describe "Execute SyncConnections" $ do
    it "should save the connections" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        conns <- embed $ generate arbitrary

        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetConnectionsReturns (\_ _ _ -> pure (Backend.ConnectionList conns False))

        mockMany @MockedEffects . assertNoError . assertNoRandomness $ do
          Execute.execute Opts.SyncConnections

        saveConnsCalls <- Store.mockSaveConnectionsCalls
        embed $ saveConnsCalls `shouldBe` [conns]

  describe "Execute ListConnections" $ do
    it "should list connections from the store" $
      runM . evalMocks @MockedEffects $ do
        conns <- embed $ generate arbitrary
        Store.mockGetConnectionsReturns $ pure conns

        returnedConns <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute (Opts.ListConnections (Opts.ListConnsOptions Nothing))

        embed $ returnedConns `shouldBe` conns

  describe "Execute Connect" $ do
    it "should send the connection request to the backend" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        Store.mockGetCredsReturns (pure (Just creds))

        req <- embed $ generate arbitrary
        mockMany @MockedEffects . assertNoError . assertNoRandomness $ Execute.execute (Opts.Connect req)

        connectCalls' <- Backend.mockConnectCalls
        embed $ connectCalls' `shouldBe` [(creds, req)]

    it "should error when user is not logged in" $
      runM . evalMocks @MockedEffects $ do
        req <- embed $ generate arbitrary
        assertNoUnauthenticatedAccess . mockMany @MockedEffects . assertNoRandomness $
          Execute.execute (Opts.Connect req)

  describe "Execute UpdateConnection" $ do
    it "should update connection in the backend" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        Store.mockGetCredsReturns (pure (Just creds))

        user <- embed $ generate arbitrary
        rel <- embed $ generate arbitrary
        mockMany @MockedEffects . assertNoError . assertNoRandomness $
          Execute.execute (Opts.UpdateConnection (Opts.UpdateConnOptions user rel))

        updateCalls <- Backend.mockUpdateConnectionCalls
        embed $ updateCalls `shouldBe` [(creds, user, rel)]

    it "should error when user is not logged in" $
      runM . evalMocks @MockedEffects $ do
        user <- embed $ generate arbitrary
        rel <- embed $ generate arbitrary
        assertNoUnauthenticatedAccess . mockMany @MockedEffects . assertNoRandomness $
          Execute.execute (Opts.UpdateConnection (Opts.UpdateConnOptions user rel))

  describe "Execute SetHandle" $ do
    it "should set handle in the backend" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        Store.mockGetCredsReturns (pure (Just creds))

        handle <- embed $ generate arbitrary
        mockMany @MockedEffects . assertNoError . assertNoRandomness $
          Execute.execute (Opts.SetHandle handle)

        calls <- Backend.mockSetHandleCalls
        embed $ calls `shouldBe` [(creds, handle)]

    it "should error when user is not logged in" $
      runM . evalMocks @MockedEffects $ do
        handle <- embed $ generate arbitrary
        assertNoUnauthenticatedAccess . mockMany @MockedEffects . assertNoRandomness $
          Execute.execute (Opts.SetHandle handle)
  describe "Execute SendMessage" $ do
    it "should error when user is not logged in" $
      runM . evalMocks @MockedEffects $ do
        convId <- embed $ generate arbitrary
        text <- embed $ generate arbitrary
        assertNoUnauthenticatedAccess . mockMany @MockedEffects . assertNoRandomness $
          Execute.execute (Opts.SendMessage (Opts.SendMessageOptions convId text))

    it "should try to send message to no clients to discover clients" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        clientId <- embed $ generate arbitrary

        Store.mockGetCredsReturns (pure (Just creds))
        Store.mockGetClientIdReturns (pure (Just clientId))
        Backend.mockSendOtrMessageReturns (\_ _ _ -> pure Backend.OtrMessageResponseSuccess)

        UUIDGen.mockGenV4Returns UUIDv4.nextRandom

        convId <- embed $ generate arbitrary
        text <- embed $ generate arbitrary
        mockMany @MockedEffects . assertNoError . assertNoRandomness $
          Execute.execute (Opts.SendMessage (Opts.SendMessageOptions convId text))

        Backend.mockSendOtrMessageCalls >>= \case
          [(actualCreds, actualConv, actualOtrMsg)] -> embed $ do
            actualCreds `shouldBe` creds
            actualConv `shouldBe` convId
            Backend.nomSender actualOtrMsg `shouldBe` clientId
            Backend.recipients (Backend.nomRecipients actualOtrMsg) `shouldBe` mempty
          calls -> embed $ expectationFailure $ "Expected exactly one call to send otr message, but got: " <> show calls

    it "should discover clients, encrypt for them and send" $
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        senderClientId <- embed $ generate arbitrary

        (receiverUser, receiverClient) <- embed $ generate arbitrary
        receiverBox <- getTempCBox
        receiverPrekey <- newPrekeyWithBox receiverBox 0x1432

        Store.mockGetCredsReturns (pure (Just creds))
        Store.mockGetClientIdReturns (pure (Just senderClientId))

        let missing = Backend.UserClients $ Map.singleton receiverUser [receiverClient]
        Backend.mockSendOtrMessageReturns
          ( \_ _ newOtr -> do
              let cm = Backend.ClientMismatch mempty undefined missing mempty
              if Map.null . Backend.userClientMap . Backend.recipients . Backend.nomRecipients $ newOtr
                then pure $ Backend.OtrMessageResponseClientMismatch cm
                else pure Backend.OtrMessageResponseSuccess
          )

        Backend.mockGetPrekeyBundlesReturns
          ( \_ _ ->
              pure $ Backend.PrekeyBundles $ Backend.UserClientMap $ Map.singleton receiverUser $ Map.fromList [(receiverClient, receiverPrekey)]
          )

        msgId <- embed UUIDv4.nextRandom
        UUIDGen.mockGenV4Returns (pure msgId)

        convId <- embed $ generate arbitrary
        plainMessage <- embed $ generate arbitrary
        encBox <- getTempCBox
        mockMany @MockedEffects . assertNoError . assertNoRandomness . CryptoBoxFFI.run encBox $
          Execute.execute (Opts.SendMessage (Opts.SendMessageOptions convId plainMessage))

        Backend.mockGetPrekeyBundlesCalls >>= \calls ->
          embed $ calls `shouldBe` [(creds, missing)]

        let expectedMessage :: M.GenericMessage =
              Proto.defMessage
                & #messageId .~ UUID.toText msgId
                & #maybe'content .~ Just plainMessage
        Backend.mockSendOtrMessageCalls >>= \case
          [(call1Creds, call1Conv, call1Otr), (call2Creds, call2Conv, call2Otr)] ->
            embed $ do
              call1Creds `shouldBe` creds
              call2Creds `shouldBe` creds

              call1Conv `shouldBe` convId
              call2Conv `shouldBe` convId

              Backend.nomSender call1Otr `shouldBe` senderClientId
              Backend.nomSender call2Otr `shouldBe` senderClientId

              Backend.nomRecipients call1Otr `shouldBe` Backend.Recipients mempty
              let recipients = Backend.userClientMap $ Backend.recipients $ Backend.nomRecipients call2Otr
              (Base64ByteString encrypted) <- assertLookup receiverClient =<< assertLookup receiverUser recipients
              runM $ do
                (_, decrypted) <- decryptWithBox receiverBox (CBox.SID "sessionU1C1") encrypted
                embed $ decrypted `shouldBe` Proto.encodeMessage expectedMessage
          calls -> embed $ expectationFailure $ "Expected exactly two call to send otr message, but got: " <> show (length calls) <> "\n" <> show calls

  describe "Execute ListMessages" $ do
    it "should list last n messages" $
      runM . evalMocks @MockedEffects $ do
        (msgs, conv, n) <- embed $ generate arbitrary
        Store.mockGetLastNMessagesReturns (\_ _ -> pure msgs)

        returnedMsgs <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute (Opts.ListMessages (Opts.ListMessagesOptions conv n))

        getCalls <- Store.mockGetLastNMessagesCalls
        embed $ do
          getCalls `shouldBe` [(conv, n)]
          returnedMsgs `shouldBe` msgs
  describe "Execute SyncSelf" $ do
    it "should get self from backend and save it" $ do
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        self <- embed $ generate arbitrary
        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetSelfReturns (const $ pure self)

        mockMany @MockedEffects . assertNoError . assertNoRandomness $
          Execute.execute Opts.SyncSelf

        getCalls <- Backend.mockGetSelfCalls
        saveCalls <- Store.mockSaveSelfCalls
        embed $ do
          getCalls `shouldBe` [creds]
          saveCalls `shouldBe` [self]

  describe "Execute GetSelf" $ do
    it "should get self from store" $ do
      runM . evalMocks @MockedEffects $ do
        self <- embed $ generate arbitrary
        Store.mockGetSelfReturns (pure (Just self))

        returned <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute (Opts.GetSelf (Opts.GetSelfOptions False))

        embed $ returned `shouldBe` self

    it "should respect the force-refresh" $ do
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        self <- embed $ generate arbitrary
        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetSelfReturns (const $ pure self)

        returned <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute (Opts.GetSelf (Opts.GetSelfOptions True))

        getCalls <- Backend.mockGetSelfCalls
        saveCalls <- Store.mockSaveSelfCalls
        embed $ do
          getCalls `shouldBe` [creds]
          saveCalls `shouldBe` [self]
          returned `shouldBe` self

    it "should sync if needed" $ do
      runM . evalMocks @MockedEffects $ do
        creds <- embed $ generate arbitrary
        self <- embed $ generate arbitrary
        Store.mockGetSelfReturns (pure Nothing)
        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetSelfReturns (const (pure self))

        returned <-
          mockMany @MockedEffects . assertNoError . assertNoRandomness $
            Execute.execute (Opts.GetSelf (Opts.GetSelfOptions False))

        getFromBackendCalls <- Backend.mockGetSelfCalls
        saveCalls <- Store.mockSaveSelfCalls
        embed $ do
          getFromBackendCalls `shouldBe` [creds]
          saveCalls `shouldBe` [self]
          returned `shouldBe` self

assertGenKeysAndRegisterClient ::
  (Members [MockImpl Backend IO, MockImpl CryptoBox IO, Embed IO] r, HasCallStack) =>
  Backend.ServerCredential ->
  Maybe Text ->
  Sem r ()
assertGenKeysAndRegisterClient expectedCred maybePassword = do
  -- Generate 100 prekeys
  newPrekeyCalls' <- mockNewPrekeyCalls
  embed $ newPrekeyCalls' `shouldBe` ([0 .. 99] <> [maxBound])
  -- Make register call to backend
  registerClientCalls' <- mockRegisterClientCalls
  embed $ length registerClientCalls' `shouldBe` 1
  let (serverCred, Client.NewClient {..}) = head registerClientCalls'
  embed $ do
    serverCred `shouldBe` expectedCred
    maybe (pure ()) (newClientPassword `shouldBe`) maybePassword
    newClientCookie `shouldBe` "wire-cli-cookie-label"
    newClientModel `shouldBe` "wire-cli"
    newClientClass `shouldBe` Client.Desktop
    newClientType `shouldBe` Client.Permanent
    newClientLabel `shouldBe` "wire-cli"
    length newClientPrekeys `shouldBe` 100
