module Wire.CLI.ConvSpec where

import Polysemy
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Conv as Conv
import Wire.CLI.Mocks.Backend
import Wire.CLI.Mocks.Store
import Wire.CLI.Store (Store)
import Wire.CLI.TestUtil
import Wire.API.Conversation
import Data.Qualified

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = describe "Conversations" $ do
  describe "sync" $ do
    it "should get conversations and save them" $
      runM . evalMocks @'[Backend, Store] $ do
        creds <- embed $ generate arbitrary
        convs <- embed $ generate arbitrary
        mockGetCredsReturns $ pure $ Just creds
        mockListConvsReturns $ \_ _ _ -> pure $ ConversationList convs False

        assertNoError $ mockMany @'[Backend, Store] Conv.sync

        -- Should ask for 500 conversation
        listCalls <- mockListConvsCalls
        embed $ listCalls `shouldBe` [(creds, Nothing, Nothing)]
        -- Save all the returned conversations
        saveConvs <- mockSaveConvsCalls
        embed $ saveConvs `shouldBe` [convs]

    it "should error if there are no creds" $
      runM . evalMocks @'[Backend, Store] $
        assertNoUnauthenticatedAccess $
          mockMany @'[Backend, Store] Conv.sync

    it "should page through conversations" $
      runM . evalMocks @'[Backend, Store] $ do
        creds <- embed $ generate arbitrary
        convsBatch1ExceptLast <- embed $ generate arbitrary
        convsBatch1Last :: Conversation <- embed $ generate arbitrary
        let lastConvId = qUnqualified $ cnvQualifiedId convsBatch1Last
        convsBatch2 <- embed $ generate arbitrary
        mockGetCredsReturns $ pure $ Just creds
        mockListConvsReturns $ \_ _ -> \case
          Nothing -> pure $ ConversationList (convsBatch1ExceptLast ++ [convsBatch1Last]) True
          Just _ -> pure $ ConversationList convsBatch2 False

        assertNoError $ mockMany @'[Backend, Store] Conv.sync

        listCalls <- mockListConvsCalls
        embed $ listCalls `shouldBe` [(creds, Nothing, Nothing), (creds, Nothing, Just lastConvId)]

        saveConvs <- mockSaveConvsCalls
        embed $ saveConvs `shouldBe` [convsBatch1ExceptLast ++ [convsBatch1Last] ++ convsBatch2]
