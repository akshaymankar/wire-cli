{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Conv where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Wire.CLI.Util.JSONStrategy

newtype ConvId = ConvId Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype UserId = UserId Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype ServiceId = ServiceId Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype ProviderId = ProviderId Text
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
  deriving (Show, Eq)

instance FromJSON ConvType where
  parseJSON = Aeson.withScientific "ConvType" $ \case
    0 -> pure Regular
    1 -> pure Self
    2 -> pure One2One
    3 -> pure Connect
    n -> fail $ "Conv type can only be one of 0,1,2,3, got: " <> show n

instance ToJSON ConvType where
  toJSON = Aeson.Number . \case
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
    selfMemberService :: Maybe ServiceRef,
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
    otherMemberService :: Maybe ServiceRef
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "otherMember" OtherMember

data ServiceRef = ServiceRef
  { serviceRefId :: ServiceId,
    serviceRefProvider :: ProviderId
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "serviceRef" ServiceRef
