{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.CLI.Backend.Arbitrary where

import qualified Data.Aeson as Aeson
import Data.List.NonEmpty
import Generic.Random
import qualified Network.HTTP.Client as HTTP
import Network.URI.Arbitrary ()
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Event
import Wire.CLI.Backend.Notification
import Wire.CLI.Backend.User
import Wire.CLI.Properties
import Wire.CLI.Util.ByteStringJSON

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

deriving newtype instance Arbitrary Base64ByteString

deriving newtype instance Arbitrary FolderId

deriving newtype instance Arbitrary NotificationId

deriving via (GenericUniform AccessToken) instance Arbitrary AccessToken

deriving via (GenericUniform ConnectionEvent) instance Arbitrary ConnectionEvent

deriving via (GenericUniform Credential) instance Arbitrary Credential

deriving via (GenericUniform DeletePropertyEvent) instance Arbitrary DeletePropertyEvent

deriving via (GenericUniform Event) instance Arbitrary Event

deriving via (GenericUniform Folder) instance Arbitrary Folder

deriving via (GenericUniform FolderType) instance Arbitrary FolderType

deriving via (GenericUniform Notification) instance Arbitrary Notification

deriving via (GenericUniform Notifications) instance Arbitrary Notifications

deriving via (GenericUniform PushToken) instance Arbitrary PushToken

deriving via (GenericUniform PushTokenRemoveEvent) instance Arbitrary PushTokenRemoveEvent

deriving via (GenericUniform ReadReciept) instance Arbitrary ReadReciept

deriving via (GenericUniform ServerCredential) instance Arbitrary ServerCredential

deriving via (GenericUniform SetPropertyEvent) instance Arbitrary SetPropertyEvent

deriving via (GenericUniform TokenType) instance Arbitrary TokenType

deriving via (GenericUniform UserIdentityRemove) instance Arbitrary UserIdentityRemove

deriving via (GenericUniform UserUpdate) instance Arbitrary UserUpdate

deriving via (GenericUniform UserEvent) instance Arbitrary UserEvent

deriving via (GenericUniform UserPropertyEvent) instance Arbitrary UserPropertyEvent

deriving via (GenericUniform WireCookie) instance Arbitrary WireCookie

instance Arbitrary ExtensibleEvent where
  arbitrary =
    oneof
      [ KnownEvent <$> arbitrary,
        pure (UnknownEvent "some parse errror" (Aeson.String "very arbitrary"))
      ]

newtype GenericUniform a = GenericUniform {getGenericUniform :: a}

instance (GArbitrary CustomSizedOpts a, GUniformWeight a) => Arbitrary (GenericUniform a) where
  arbitrary =
    GenericUniform
      <$> genericArbitraryWith @CustomSizedOpts @a customSizedOpts uniform

-- | We want plug in custom generators for all occurences of '[]' and 'List1'.
type CustomSizedOpts =
  Options
    'INCOHERENT
    'Sized
    (Gen1 [] :+ Gen1 NonEmpty :+ ())

customSizedOpts :: CustomSizedOpts
customSizedOpts =
  setGenerators
    (Gen1 listOf' :+ Gen1 nonEmptyListOf' :+ ())
    sizedOpts

nonEmptyListOf' :: Gen a -> Gen (NonEmpty a)
nonEmptyListOf' g = (:|) <$> g <*> listOf' g

-- | Generate unprocessed notifications to ensure least amount of mocking needed
unprocessedNotification :: Gen Notification
unprocessedNotification =
  Notification
    <$> arbitrary
    <*> pure (UnknownEvent "some parse error" (Aeson.String "some event") :| [])
