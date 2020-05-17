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
  { conversations :: [Conv],
    hasMore :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy Convs

data Conv = Conv
  { id :: ConvId,
    creator :: UserId,
    members :: ConvMembers,
    name :: Maybe Text,
    typ :: ConvType,
    messageTimer :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy Conv

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
  { self :: SelfMember,
    others :: [OtherMember]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy ConvMembers

data SelfMember = SelfMember
  { id :: UserId,
    service :: Maybe ServiceRef,
    hidden :: Maybe Bool,
    hiddenRef :: Maybe Text,
    otrMuted :: Maybe Bool,
    otrMutedRef :: Maybe Text,
    otrArchived :: Maybe Bool,
    otrArchivedRef :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy SelfMember

data OtherMember = OtherMember
  { id :: UserId,
    service :: Maybe ServiceRef
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy OtherMember

data ServiceRef = ServiceRef
  { id :: ServiceId,
    provider :: ProviderId
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy ServiceRef
