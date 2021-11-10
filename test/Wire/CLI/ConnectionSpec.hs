module Wire.CLI.ConnectionSpec where

import Data.Functor ((<&>))
import Polysemy
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.API.Connection
import Wire.API.Routes.MultiTablePaging
import Wire.CLI.APIArbitrary ()
import Wire.CLI.Backend (Backend)
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Connection as Connection
import Wire.CLI.Mocks.Backend as Backend
import Wire.CLI.Mocks.Store as Store
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import Wire.CLI.TestUtil

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = describe "Connections" $ do
  describe "sync" $ do
    it "should save the connections" $
      runM . evalMocks @[Backend, Store] $ do
        creds <- embed $ generate arbitrary
        conns <- embed $ generate arbitrary
        firstPageState <- embed $ generate arbitrary

        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetConnectionsReturns (\_ _ _ -> pure (MultiTablePage conns False firstPageState))

        mockMany @[Backend, Store] . assertNoError . assertNoRandomness $ Connection.sync

        getConnsCalls <- Backend.mockGetConnectionsCalls
        saveConnsCalls <- Store.mockSaveConnectionsCalls
        embed $ do
          getConnsCalls `shouldBe` [(creds, Nothing, Nothing)]
          saveConnsCalls `shouldBe` [conns]

    it "should error when user is not logged in" $
      runM . evalMocks @[Backend, Store] $
        assertNoUnauthenticatedAccess
          . mockMany @[Backend, Store]
          . assertNoRandomness
          $ Connection.sync

    it "should page through connections" $
      runM . evalMocks @[Backend, Store] $ do
        creds <- embed $ generate arbitrary
        conns1 <- embed $ generate arbitrary
        conns2 <- embed $ generate arbitrary
        firstPageState <- embed $ generate arbitrary
        lastPageState <- embed $ generate arbitrary

        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetConnectionsReturns
          ( \_ _ start ->
              case start of
                Nothing -> pure (MultiTablePage conns1  True firstPageState)
                Just _ -> pure (MultiTablePage conns2 False lastPageState)
          )

        mockMany @[Backend, Store] . assertNoError . assertNoRandomness $ Connection.sync

        getConnsCalls <- Backend.mockGetConnectionsCalls
        saveConnsCalls <- Store.mockSaveConnectionsCalls
        let hackState (MultiTablePagingState t bs) = MultiTablePagingState t bs
        embed $ do
          getConnsCalls
            `shouldBe` [ (creds, Nothing, Nothing), -- Start with no state
                         (creds, Nothing, Just (hackState firstPageState)) -- Send state from first page to get the second page
                       ]
          saveConnsCalls `shouldBe` [conns1 <> conns2]

  describe "list" $ do
    it "should filter by relation status if provided" $
      runM . evalMock @Store $ do
        pendingConns <- embed (generate arbitrary) <&> map (\conn -> conn {ucStatus = Pending})
        otherConns <-
          embed (generate arbitrary)
            <&> map
              ( \conn ->
                  if ucStatus conn == Pending
                    then conn {ucStatus = Accepted}
                    else conn
              )
        Store.mockGetConnectionsReturns (pure $ pendingConns <> otherConns)

        listed <- mock @Store $ Connection.list (Opts.ListConnsOptions (Just Pending))
        embed $ listed `shouldBe` pendingConns
