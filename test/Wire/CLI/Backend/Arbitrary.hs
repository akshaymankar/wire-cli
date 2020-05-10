{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.CLI.Backend.Arbitrary where

import qualified Network.HTTP.Client as HTTP
import Network.URI.Arbitrary ()
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Conv
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

deriving instance Arbitrary ConvId

deriving instance Arbitrary UserId

deriving instance Arbitrary ServiceId

deriving instance Arbitrary ProviderId

instance Arbitrary ServiceRef where
  arbitrary = ServiceRef <$> arbitrary <*> arbitrary

instance Arbitrary OtherMember where
  arbitrary = OtherMember <$> arbitrary <*> arbitrary

instance Arbitrary SelfMember where
  arbitrary =
    SelfMember
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ConvMembers where
  arbitrary = ConvMembers <$> arbitrary <*> arbitrary

instance Arbitrary ConvType where
  arbitrary = elements [Regular, Self, One2One, Connect]

instance Arbitrary Conv where
  arbitrary =
    Conv
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Convs where
  arbitrary =
    Convs
      <$> arbitrary
      <*> arbitrary

instance Arbitrary ServerCredential where
  arbitrary =
    ServerCredential <$> arbitrary <*> arbitrary
