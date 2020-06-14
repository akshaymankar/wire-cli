module Wire.CLI.NotificationSpec where

import qualified Data.UUID as UUID
import Polysemy
import qualified Polysemy.Error as Error
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.Backend.Notification
import qualified Wire.CLI.Error as WErr
import Wire.CLI.Mocks.Backend
import Wire.CLI.Mocks.Store
import qualified Wire.CLI.Notification as Notification
import Wire.CLI.Store (Store)
import Wire.CLI.TestUtil

type MockedEffects = '[Backend, Store]

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
{-# ANN spec ("HLint: ignore Reduce duplication" :: String) #-}
spec :: Spec
spec = describe "Notification" $ do
  describe "sync" $ do
    it "should error gracefully when user is not logged in" $ runM . evalMocks @MockedEffects
      $ assertNoUnauthenticatedAccess
      $ mockMany @MockedEffects Notification.sync

    it "should error gracefully when a client-id is not found" $ runM . evalMocks @MockedEffects $ do
      creds <- embed $ generate arbitrary
      mockGetCredsReturns (pure (Just creds))
      mockGetClientIdReturns (pure Nothing)

      eitherErr <- mockMany @MockedEffects . Error.runError $ Notification.sync

      embed $ eitherErr `shouldBe` Left (WErr.ErrorInvalidState WErr.NoClientFound)

    it "should use nil UUID as last notification id when it doesn't exist in the store" $ runM . evalMocks @MockedEffects $ do
      creds <- embed $ generate arbitrary
      client <- embed $ generate arbitrary
      notifs <- embed $ Notifications False <$> generate arbitrary
      mockGetCredsReturns (pure (Just creds))
      mockGetClientIdReturns (pure (Just client))
      mockGetLastNotificationIdReturns (pure Nothing)
      mockGetNotificationsReturns
        ( \_ _ _ _ ->
            pure (NotificationGapDoesNotExist, notifs)
        )

      mockMany @MockedEffects . assertNoError $ Notification.sync

      getNotifCalls <- mockGetNotificationsCalls
      embed $ getNotifCalls `shouldBe` [(creds, 1000, client, NotificationId UUID.nil)]

    it "should page through notifications" $ runM . evalMocks @MockedEffects $ do
      creds <- embed $ generate arbitrary
      client <- embed $ generate arbitrary
      notifsPage1 <- embed $ generate arbitrary
      lastNotifPage1 <- embed $ generate arbitrary
      notifsPage2 <- embed $ generate arbitrary
      lastNotifPage2 <- embed $ generate arbitrary
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
