{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.APIArbitrary where

import Test.QuickCheck
import Wire.API.Routes.MultiTablePaging

instance Arbitrary LocalOrRemoteTable where
  arbitrary = elements [PagingLocals, PagingRemotes]

instance Arbitrary tables => Arbitrary (MultiTablePagingState name tables) where
  arbitrary = MultiTablePagingState <$> arbitrary <*> arbitrary
