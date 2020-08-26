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
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.CommonTypes
import Wire.CLI.Backend.Connection
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Event
import Wire.CLI.Backend.Notification
import Wire.CLI.Backend.Prekey
import Wire.CLI.Backend.Search
import Wire.CLI.Backend.Service
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

deriving newtype instance Arbitrary AssetId

deriving newtype instance Arbitrary Base64ByteString

deriving newtype instance Arbitrary ClientId

deriving newtype instance Arbitrary ConvId

-- TODO: retrict length to 256 characters
deriving newtype instance Arbitrary ConnectionMessage

-- TODO: make it look like an email
deriving newtype instance Arbitrary Email

deriving newtype instance Arbitrary FolderId

deriving newtype instance Arbitrary Handle

deriving newtype instance Arbitrary Name

deriving newtype instance Arbitrary NotificationId

-- TODO: make it look like a phone number
deriving newtype instance Arbitrary PhoneNumber

deriving newtype instance Arbitrary ProviderId

deriving newtype instance Arbitrary SSOId

deriving newtype instance Arbitrary ServiceId

deriving newtype instance Arbitrary Tag

deriving newtype instance Arbitrary TeamId

deriving newtype instance Arbitrary TrackingId

deriving newtype instance Arbitrary UserId

deriving via (GenericUniform Access) instance Arbitrary Access

deriving via (GenericUniform AccessEvent) instance Arbitrary AccessEvent

deriving via (GenericUniform AccessRole) instance Arbitrary AccessRole

deriving via (GenericUniform AccessToken) instance Arbitrary AccessToken

deriving via (GenericUniform Client) instance Arbitrary Client

deriving via (GenericUniform ClientClass) instance Arbitrary ClientClass

deriving via (GenericUniform ClientType) instance Arbitrary ClientType

deriving via (GenericUniform ConnectRequestEvent) instance Arbitrary ConnectRequestEvent

deriving via (GenericUniform Connection) instance Arbitrary Connection

deriving via (GenericUniform ConnectionEvent) instance Arbitrary ConnectionEvent

deriving via (GenericUniform ConnectionRequest) instance Arbitrary ConnectionRequest

deriving via (GenericUniform Conv) instance Arbitrary Conv

deriving via (GenericUniform ConvCreateEvent) instance Arbitrary ConvCreateEvent

deriving via (GenericUniform ConvEvent) instance Arbitrary ConvEvent

deriving via (GenericUniform ConvEventData) instance Arbitrary ConvEventData

deriving via (GenericUniform ConvMembers) instance Arbitrary ConvMembers

deriving via (GenericUniform ConvRecieptMode) instance Arbitrary ConvRecieptMode

deriving via (GenericUniform ConvRole) instance Arbitrary ConvRole

deriving via (GenericUniform ConvType) instance Arbitrary ConvType

deriving via (GenericUniform Convs) instance Arbitrary Convs

deriving via (GenericUniform Credential) instance Arbitrary Credential

deriving via (GenericUniform DecryptionErrorData) instance Arbitrary DecryptionErrorData

deriving via (GenericUniform DeletePropertyEvent) instance Arbitrary DeletePropertyEvent

deriving via (GenericUniform Event) instance Arbitrary Event

deriving via (GenericUniform Folder) instance Arbitrary Folder

deriving via (GenericUniform FolderType) instance Arbitrary FolderType

deriving via (GenericUniform IdentityChangedData) instance Arbitrary IdentityChangedData

deriving via (GenericUniform ManagedBy) instance Arbitrary ManagedBy

deriving via (GenericUniform MemberJoinEvent) instance Arbitrary MemberJoinEvent

deriving via (GenericUniform MemberUpdateEvent) instance Arbitrary MemberUpdateEvent

deriving via (GenericUniform MessageTimer) instance Arbitrary MessageTimer

deriving via (GenericUniform MutedStatus) instance Arbitrary MutedStatus

deriving via (GenericUniform NewClient) instance Arbitrary NewClient

deriving via (GenericUniform Notification) instance Arbitrary Notification

deriving via (GenericUniform Notifications) instance Arbitrary Notifications

deriving via (GenericUniform OtherMember) instance Arbitrary OtherMember

deriving via (GenericUniform OtrError) instance Arbitrary OtrError

deriving via (GenericUniform OtrMessage) instance Arbitrary OtrMessage

deriving via (GenericUniform Prekey) instance Arbitrary Prekey

deriving via (GenericUniform ProfilePicture) instance Arbitrary ProfilePicture

deriving via (GenericUniform PushToken) instance Arbitrary PushToken

deriving via (GenericUniform PushTokenRemoveEvent) instance Arbitrary PushTokenRemoveEvent

deriving via (GenericUniform ReadReciept) instance Arbitrary ReadReciept

deriving via (GenericUniform Relation) instance Arbitrary Relation

deriving via (GenericUniform SearchResult) instance Arbitrary SearchResult

deriving via (GenericUniform SearchResults) instance Arbitrary SearchResults

deriving via (GenericUniform SelfMember) instance Arbitrary SelfMember

deriving via (GenericUniform ServerCredential) instance Arbitrary ServerCredential

deriving via (GenericUniform Service) instance Arbitrary Service

deriving via (GenericUniform SetPropertyEvent) instance Arbitrary SetPropertyEvent

deriving via (GenericUniform TeamData) instance Arbitrary TeamData

deriving via (GenericUniform TeamEvent) instance Arbitrary TeamEvent

deriving via (GenericUniform TeamEventData) instance Arbitrary TeamEventData

deriving via (GenericUniform TokenType) instance Arbitrary TokenType

deriving via (GenericUniform TypingStatus) instance Arbitrary TypingStatus

deriving via (GenericUniform User) instance Arbitrary User

deriving via (GenericUniform UserEvent) instance Arbitrary UserEvent

deriving via (GenericUniform UserField) instance Arbitrary UserField

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

type CustomSizedOpts =
  Options
    'Sized
    (Gen1 [] :+ Gen1 NonEmpty :+ ())

customSizedOpts :: CustomSizedOpts
customSizedOpts =
  setGenerators
    (Gen1 listOf' :+ Gen1 nonEmptyListOf' :+ ())
    sizedOpts

nonEmptyListOf' :: Gen a -> Gen (NonEmpty a)
nonEmptyListOf' g = (:|) <$> g <*> listOf' g
