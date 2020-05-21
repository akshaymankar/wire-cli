{-# LANGUAGE RecordWildCards #-}

module Wire.CLI.ExecuteSpec where

import qualified Network.URI as URI
import Polysemy
import qualified Polysemy.Error as Error
import Polysemy.Internal (send)
import qualified System.CryptoBox as CBox
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Backend.Client as Client
import Wire.CLI.CryptoBox (CryptoBox)
import Wire.CLI.Display (Display)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Execute as Execute
import Wire.CLI.Mocks
import Wire.CLI.Mocks.Display ()
import qualified Wire.CLI.Mocks.Display as Display
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import Wire.CLI.TestUtil

type MockedEffects = '[Backend, Store, CryptoBox, Display]

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = do
  describe "Execute Login" $ do
    let Just server = URI.parseURI "https://be.example.com"
    let loginOpts = Opts.LoginOptions server "handle" "pwwpw"
    let loginCommand = Opts.Login loginOpts
    it "should login and store creds" $ runM . evalMocks @MockedEffects $ do
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

      -- execute the command
      mockMany @'[Backend, Store, CryptoBox] . assertNoError $
        Execute.execute loginCommand

      -- Expectations:
      -- Call backend
      loginCalls' <- mockLoginCalls
      embed $ loginCalls' `shouldBe` [loginOpts]
      -- Store creds
      saveCredsCalls' <- mockSaveCredsCalls
      embed $ saveCredsCalls' `shouldBe` [Backend.ServerCredential server cred]
      -- Register Client
      -- Generate 100 prekeys
      newPrekeyCalls' <- mockNewPrekeyCalls
      embed $ newPrekeyCalls' `shouldBe` ([0 .. 99] <> [maxBound])
      -- Make register call to backend
      registerClientCalls' <- mockRegisterClientCalls
      embed $ length registerClientCalls' `shouldBe` 1
      let (serverCred, Client.NewClient {..}) = head registerClientCalls'
      embed $ serverCred `shouldBe` Backend.ServerCredential server cred
      embed $ newClientCookie `shouldBe` "wire-cli-cookie-label"
      embed $ newClientPassword `shouldBe` "pwwpw"
      embed $ newClientModel `shouldBe` "wire-cli"
      embed $ newClientClass `shouldBe` Client.Desktop
      embed $ newClientType `shouldBe` Client.Permanent
      embed $ newClientLabel `shouldBe` "wire-cli"
      embed $ length newClientPrekeys `shouldBe` 100
      -- Save the returned client id
      saveClientIdCalls' <- mockSaveClientIdCalls
      embed $ saveClientIdCalls' `shouldBe` [clientId]

    it "should error when login fails" $ runM . evalMocks @MockedEffects $ do
      mockLoginReturns (const $ pure $ Backend.LoginFailure "something failed")

      eitherErr <- mockMany @MockedEffects . Error.runError $ Execute.execute loginCommand

      embed $ eitherErr `shouldBe` Left (WireCLIError.LoginFailed "something failed")

  describe "Execute SyncConvs" $ do
    it "should get convs from the server and store them" $ runM . evalMocks @MockedEffects $ do
      convs <- embed $ generate arbitrary
      creds <- embed $ generate arbitrary
      mockGetCredsReturns $ pure (Just creds)
      mockListConvsReturns $ \_ _ _ -> do
        pure $ Backend.Convs convs False

      mockMany @MockedEffects . assertNoError $
        Execute.execute Opts.SyncConvs

      saveConvs <- mockSaveConvsCalls
      embed $ saveConvs `shouldBe` [convs]

  describe "Execute ListConvs" $ do
    it "should list convs from the store" $ runM . evalMocks @MockedEffects $ do
      convs <- embed $ generate arbitrary
      mockGetConvsReturns $ pure (Just convs)

      mockMany @MockedEffects . assertNoError $
        Execute.execute (Opts.ListConvs (send . Display.MockListConvs))

      listConvs <- Display.mockListConvsCalls
      embed $ listConvs `shouldBe` [convs]

  describe "Execute SyncNotification" $ do
    it "should get notifications and store the last one" $ runM . evalMocks @MockedEffects $ do
      creds <- embed $ generate arbitrary
      mockGetCredsReturns $ pure (Just creds)
      lastNotification <- embed $ generate arbitrary
      allButLastNotification <- embed $ generate arbitrary
      let notifications = Backend.Notifications False (allButLastNotification <> [lastNotification])
      previousLastNotificationId <- embed $ generate arbitrary
      clientId <- embed $ generate arbitrary

      mockGetNotificationsReturns $ \_ _ _ _ -> pure (Backend.NotificationGapExists, notifications)
      mockGetLastNotificationIdReturns $ pure (Just previousLastNotificationId)
      mockGetClientIdReturns $ pure (Just clientId)

      mockMany @MockedEffects . assertNoError $
        Execute.execute Opts.SyncNotifications

      notifCalls <- mockGetNotificationsCalls
      embed $ notifCalls `shouldBe` [(creds, 1000, clientId, previousLastNotificationId)]
      saveNotifCalls <- mockSaveLastNotificationIdCalls
      embed $ saveNotifCalls `shouldBe` [Backend.notificationId lastNotification]
