{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Wire.CLI.Backend.User where

import Data.Aeson (FromJSONKey, ToJSONKey)
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

newtype SSOId = SSOId Text
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
    userUpdateSsoId :: Maybe SSOId,
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
  deriving (FromJSON) via JSONStrategy "user" User

data Asset = Asset
  { assetKey :: Text,
    assetSize :: AssetSize
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON) via JSONStrategy "asset" Asset

data AssetSize
  = Complete
  | Preview
  deriving stock (Eq, Show, Generic)

instance FromJSON AssetSize where
  parseJSON = Aeson.withText "AssetSize" $ \case
    "complete" -> pure Complete
    "preview" -> pure Preview
    x -> fail $  "Invalid asset size: " <> show x
