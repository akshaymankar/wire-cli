module Wire.CLI.ConvSpec where

import Polysemy
import qualified Polysemy.Error as Error
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Conv as Conv
import Wire.CLI.Mocks.Backend
import Wire.CLI.Mocks.Store
import Wire.CLI.Store (Store)
import Wire.CLI.TestUtil

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = describe "Conversations" $ do
  describe "sync" $ do
    it "should get conversations and save them" $ runM . evalMocks @'[Backend, Store] $ do
      creds <- embed $ generate arbitrary
      convs <- embed $ generate arbitrary
      mockGetCredsReturns $ pure $ Just creds
      mockListConvsReturns $ \_ _ _ -> pure $ Backend.Convs convs False

      assertNoError $ mockMany @'[Backend, Store] Conv.sync

      -- Should ask for 500 conversation
      listCalls <- mockListConvsCalls
      embed $ listCalls `shouldBe` [(creds, 500, Nothing)]
      -- Save all the returned conversations
      saveConvs <- mockSaveConvsCalls
      embed $ saveConvs `shouldBe` [convs]

    it "should error if there are no creds" $ runM . evalMocks @'[Backend, Store] $ do
      mockGetCredsReturns $ pure Nothing

      eitherErr <- Error.runError $ mockMany @'[Backend, Store] Conv.sync

      embed $ eitherErr `shouldBe` Left WireCLIError.NotLoggedIn

      -- Shouldn't contact server
      listCalls <- mockListConvsCalls
      embed $ listCalls `shouldBe` []

    it "should page through conversations" $ runM . evalMocks @'[Backend, Store] $ do
      creds <- embed $ generate arbitrary
      convsBatch1ExceptLast <- embed $ generate arbitrary
      convsBatch1Last <- embed $ generate arbitrary
      let lastConvId = Backend.convId convsBatch1Last
      convsBatch2 <- embed $ generate arbitrary
      mockGetCredsReturns $ pure $ Just creds
      mockListConvsReturns $ \_ _ -> \case
        Nothing -> pure $ Backend.Convs (convsBatch1ExceptLast ++ [convsBatch1Last]) True
        Just _ -> pure $ Backend.Convs convsBatch2 False

      assertNoError $ mockMany @'[Backend, Store] Conv.sync

      listCalls <- mockListConvsCalls
      embed $ listCalls `shouldBe` [(creds, 500, Nothing), (creds, 500, Just lastConvId)]

      saveConvs <- mockSaveConvsCalls
      embed $ saveConvs `shouldBe` [convsBatch1ExceptLast ++ [convsBatch1Last] ++ convsBatch2]
