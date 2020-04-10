{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.CLI.Backend.Arbitrary where

import qualified Network.HTTP.Client as HTTP
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Wire.CLI.Backend.Credential

instance Arbitrary AccessToken where
  arbitrary = AccessToken <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TokenType where
  arbitrary = pure TokenTypeBearer

instance Arbitrary Credential where
  arbitrary = Credential <$> arbitrary <*> arbitrary

instance Arbitrary WireCookie where
  arbitrary = WireCookie <$> arbitrary

instance Arbitrary HTTP.Cookie where
  arbitrary =
    HTTP.Cookie
      <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
