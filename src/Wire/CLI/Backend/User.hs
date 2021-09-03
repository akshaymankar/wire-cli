{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Wire.CLI.Backend.User where

import Data.Aeson (FromJSONKey, ToJSONKey, Value (String), toJSON, object, (.=), withObject, (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Wire.CLI.Backend.CommonTypes
import Wire.CLI.Backend.Service
import Wire.CLI.Util.JSONStrategy

newtype UserId = UserId Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Ord)

newtype Email = Email Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype PhoneNumber = PhoneNumber Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype AssetId = AssetId Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Tag = Tag Text
  deriving (Show, Eq, FromJSON, ToJSON)

data ProfilePicture = ProfilePicture
  { profilePictureId :: AssetId,
    profilePictureTag :: Tag
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "profilePicture" ProfilePicture

newtype TrackingId = TrackingId Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Handle = Handle Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype TeamId = TeamId Text
  deriving (Show, Eq, FromJSON, ToJSON)

data ManagedBy
  = ManagedByWire
  | ManagedByScim
  | ManagedByOther Text
  deriving (Show, Eq, Generic)

instance FromJSON ManagedBy where
  parseJSON = Aeson.withText "ManagedBy" $ \case
    "wire" -> pure ManagedByWire
    "scim" -> pure ManagedByScim
    t -> pure $ ManagedByOther t

instance ToJSON ManagedBy where
  toJSON = \case
    ManagedByWire -> String "wire"
    ManagedByScim -> String "scim"
    ManagedByOther t -> String t

data UserField = UserField
  { userFieldType :: Text,
    userFieldValue :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "userField" UserField

data UserUpdate = UserUpdate
  { userUpdateId :: UserId,
    userUpdateName :: Maybe Name,
    userUpdateAccentId :: Maybe Int,
    userUpdateEmail :: Maybe Email,
    userUpdatePhone :: Maybe PhoneNumber,
    -- | The empty list is used to delete pictures
    userUpdatePicture :: Maybe [ProfilePicture],
    userUpdateTrackingId :: Maybe TrackingId,
    userUpdateDeleted :: Bool,
    userUpdateHandle :: Maybe Handle,
    userUpdatePrivateMode :: Maybe Bool,
    userUpdateService :: Maybe Service,
    userUpdateTeamId :: Maybe TeamId,
    userUpdateExpiresAt :: Maybe UTCTime,
    userUpdateSsoId :: Maybe UserSSOId,
    userUpdateManagedBy :: Maybe ManagedBy,
    userUpdateFields :: Maybe [UserField]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "userUpdate" UserUpdate

data User = User
  { userId :: UserId,
    userName :: Name,
    userAssets :: [Asset],
    userAccentId :: Int,
    userDeleted :: Maybe Bool,
    userService :: Maybe Service,
    userHandle :: Maybe Handle,
    userLocale :: Maybe Text,
    userExpiresAt :: Maybe UTCTime,
    userTeam :: Maybe TeamId,
    userEmail :: Maybe Email,
    userLegalholdStatus :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "user" User

data SelfUser = SelfUser
  { selfEmail :: Maybe Email,
    selfPhone :: Maybe Text,
    selfSsoId :: Maybe UserSSOId,
    selfId :: UserId,
    selfName :: Name,
    selfAssets :: [Asset],
    selfAccentId :: Int,
    selfDeleted :: Maybe Bool,
    selfLocale :: Text,
    selfHandle :: Maybe Handle,
    selfTeam :: Maybe TeamId,
    selfManaged_by :: ManagedBy
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "self" SelfUser

data UserSSOId = UserSSOId Text Text
               | UserScimExternalId Text
  deriving stock (Eq, Show, Generic)

instance ToJSON UserSSOId where
  toJSON = \case
    UserSSOId tenant subject -> object ["tenant" .= tenant, "subject" .= subject]
    UserScimExternalId eid -> object ["scim_external_id" .= eid]

instance FromJSON UserSSOId where
  parseJSON = withObject "UserSSOId" $ \obj -> do
    mtenant <- obj .:? "tenant"
    msubject <- obj .:? "subject"
    meid <- obj .:? "scim_external_id"
    case (mtenant, msubject, meid) of
      (Just tenant, Just subject, Nothing) -> pure $ UserSSOId tenant subject
      (Nothing, Nothing, Just eid) -> pure $ UserScimExternalId eid
      _ -> fail "either need tenant and subject, or scim_external_id, but not both"

data Asset = Asset
  { assetKey :: Text,
    assetSize :: AssetSize
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "asset" Asset

data AssetSize
  = Complete
  | Preview
  deriving stock (Eq, Show, Generic)

instance FromJSON AssetSize where
  parseJSON = Aeson.withText "AssetSize" $ \case
    "complete" -> pure Complete
    "preview" -> pure Preview
    x -> fail $ "Invalid asset size: " <> show x

instance ToJSON AssetSize where
  toJSON = \case
    Complete -> String "complete"
    Preview -> String "preview"
