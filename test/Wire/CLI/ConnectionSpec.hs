module Wire.CLI.ConnectionSpec where

import Data.Functor ((<&>))
import Polysemy
import Test.Hspec
import Test.Polysemy.Mock
import Test.QuickCheck
import Wire.CLI.Backend (Backend)
import Wire.CLI.Backend.Arbitrary ()
import qualified Wire.CLI.Connection as Connection
import Wire.CLI.Mocks.Backend as Backend
import Wire.CLI.Mocks.Store as Store
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import Wire.CLI.TestUtil
import Wire.API.Connection

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = describe "Connections" $ do
  describe "sync" $ do
    it "should save the connections" $
      runM . evalMocks @[Backend, Store] $ do
        creds <- embed $ generate arbitrary
        conns <- embed $ generate arbitrary

        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetConnectionsReturns (\_ _ _ -> pure (UserConnectionList conns False))

        mockMany @[Backend, Store] . assertNoError . assertNoRandomness $ Connection.sync

        saveConnsCalls <- Store.mockSaveConnectionsCalls
        embed $ saveConnsCalls `shouldBe` [conns]

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
        conns1Last <- embed $ generate arbitrary
        conns2 <- embed $ generate arbitrary

        Store.mockGetCredsReturns (pure (Just creds))
        Backend.mockGetConnectionsReturns
          ( \_ _ start ->
              case start of
                Nothing -> pure (UserConnectionList (conns1 ++ [conns1Last]) True)
                Just _ -> pure (UserConnectionList conns2 False)
          )

        mockMany @[Backend, Store] . assertNoError . assertNoRandomness $ Connection.sync

        saveConnsCalls <- Store.mockSaveConnectionsCalls
        embed $ saveConnsCalls `shouldBe` [conns1 ++ [conns1Last] ++ conns2]
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
