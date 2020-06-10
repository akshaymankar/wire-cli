{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Wire.CLI.Backend.User where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSONKey)
import Data.Text (Text)
import Data.Time (UTCTime)
import Wire.CLI.Backend.CommonTypes
import Wire.CLI.Backend.Service
import Wire.CLI.Util.JSONStrategy

newtype UserId = UserId Text
  deriving (Show, Eq, FromJSON, ToJSON, FromJSONKey, Ord)

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

data User = User
  { userId :: UserId,
    userName :: Maybe Name,
    userAccentId :: Maybe Int,
    userEmail :: Maybe Email,
    userPhone :: Maybe PhoneNumber,
    -- | The empty list is used to delete pictures
    userPicture :: Maybe [ProfilePicture],
    userTrackingId :: Maybe TrackingId,
    userDeleted :: Bool,
    userHandle :: Maybe Handle,
    userPrivateMode :: Maybe Bool,
    userService :: Maybe Service,
    userTeamId :: Maybe TeamId,
    userExpiresAt :: Maybe UTCTime,
    userSsoId :: Maybe SSOId,
    userManagedBy :: Maybe ManagedBy,
    userFields :: Maybe [UserField]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "user" User
