{-# LANGUAGE TypeApplications #-}

module Wire.CLI.Backend.TypesSpec where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.Backend.Credential

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec =
  describe "Backend.Types" $ do
    describe "Credential" $ do
      prop "json roundtrip: Credential" (roundTrip @Credential)

roundTrip :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => a -> Property
roundTrip v =
  Right v === (Aeson.parseEither Aeson.parseJSON . Aeson.toJSON) v
