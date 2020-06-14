{-# LANGUAGE TypeApplications #-}

module Wire.CLI.Backend.TypesSpec where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Connection
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Prekey
import Wire.CLI.Backend.Search

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec =
  describe "Backend.Types" $ do
    describe "Credential" $ do
      prop "json roundtrip: Credential" (roundTrip @Credential)
      prop "json roundtrip: Prekey" (roundTrip @Prekey)
      prop "json roundtrip: NewClient" (roundTrip @NewClient)
      prop "json roundtrip: Client" (roundTrip @Client)
      prop "json roundtrip: ServerCredential" (roundTrip @ServerCredential)
      prop "json roundtrip: Convs" (roundTrip @Convs)
      prop "json roundtrip: SearchResults" (roundTrip @SearchResults)
      prop "json roundtrip: Connection" (roundTrip @Connection)

roundTrip :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
roundTrip v =
  Right v === (Aeson.parseEither Aeson.parseJSON . Aeson.toJSON) v
