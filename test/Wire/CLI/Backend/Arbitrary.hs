{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.CLI.Backend.Arbitrary where

import qualified Network.HTTP.Client as HTTP
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Prekey

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

instance Arbitrary Prekey where
  arbitrary = Prekey <$> arbitrary <*> arbitrary

instance Arbitrary ClientClass where
  arbitrary = elements [Desktop, Mobile]

instance Arbitrary ClientType where
  arbitrary = elements [Permanent, Temporary]

instance Arbitrary NewClient where
  arbitrary =
    NewClient
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
