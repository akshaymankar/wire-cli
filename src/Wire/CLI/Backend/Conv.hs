{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Conv where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Wire.CLI.Backend.Service
import Wire.CLI.Backend.User (UserId)
import Wire.CLI.Util.JSONStrategy

newtype ConvId = ConvId Text
  deriving (Show, Eq, FromJSON, ToJSON)

data Convs = Convs
  { convsConversations :: [Conv],
    convsHasMore :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "convs" Convs

data Conv = Conv
  { convId :: ConvId,
    convCreator :: UserId,
    convMembers :: ConvMembers,
    convName :: Maybe Text,
    convType :: ConvType,
    convMessageTimer :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "conv" Conv

data ConvType
  = Regular
  | Self
  | One2One
  | Connect
  deriving (Show, Eq, Generic)

instance FromJSON ConvType where
  parseJSON = Aeson.withScientific "ConvType" $ \case
    0 -> pure Regular
    1 -> pure Self
    2 -> pure One2One
    3 -> pure Connect
    n -> fail $ "Conv type can only be one of 0,1,2,3, got: " <> show n

instance ToJSON ConvType where
  toJSON =
    Aeson.Number . \case
      Regular -> 0
      Self -> 1
      One2One -> 2
      Connect -> 3

data ConvMembers = ConvMembers
  { convMembersSelf :: SelfMember,
    convMembersOthers :: [OtherMember]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "convMembers" ConvMembers

data SelfMember = SelfMember
  { selfMemberId :: UserId,
    selfMemberService :: Maybe Service,
    selfMemberHidden :: Maybe Bool,
    selfMemberHiddenRef :: Maybe Text,
    selfMemberOtrMuted :: Maybe Bool,
    selfMemberOtrMutedRef :: Maybe Text,
    selfMemberOtrArchived :: Maybe Bool,
    selfMemberOtrArchivedRef :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "selfMember" SelfMember

data OtherMember = OtherMember
  { otherMemberId :: UserId,
    otherMemberService :: Maybe Service
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "otherMember" OtherMember

data Access
  = AccessInvite
  | AccessCode
  | AccessLink
  | AccessPrivate
  deriving (Show, Eq, Generic, Ord)

instance FromJSON Access where
  parseJSON = Aeson.withText "Access" $ \case
    "invite" -> pure AccessInvite
    "code" -> pure AccessCode
    "link" -> pure AccessLink
    "private" -> pure AccessPrivate
    t -> fail $ "Invalid access: " <> show t

data AccessRole
  = AccessRoleTeam
  | AccessRoleActivated
  | AccessRoleNonActivated
  | AccessRolePrivate
  deriving (Show, Eq, Generic)

instance FromJSON AccessRole where
  parseJSON = Aeson.withText "AccessRole" $ \case
    "team" -> pure AccessRoleTeam
    "activated" -> pure AccessRoleActivated
    "non_activated" -> pure AccessRoleNonActivated
    "private" -> pure AccessRolePrivate
    t -> fail $ "Invalid AccessRole: " <> show t

data ConvRole
  = ConvRoleAdmin
  | ConvRoleMember
  | ConvRoleBot
  deriving (Show, Eq, Generic)

instance FromJSON ConvRole where
  parseJSON = Aeson.withText "ConvRole" $ \case
    "wire_admin" -> pure ConvRoleAdmin
    "wire_member" -> pure ConvRoleMember
    "wire_bot" -> pure ConvRoleBot
    t -> fail $ "Invalid ConvRole: " <> show t
