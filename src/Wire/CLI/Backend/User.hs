{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Wire.CLI.Backend.User where

import Data.Handle
import Data.Id (UserId)
import Wire.API.User hiding (UserUpdate)
import Wire.CLI.Util.JSONStrategy

data UserUpdate = UserUpdate
  { userUpdateId :: UserId, --
    userUpdateName :: Maybe Name, --
    userUpdateAccentId :: Maybe ColourId, --

    -- | The empty list is used to delete pictures
    userUpdatePicture :: Maybe Pict, --
    userUpdateAssets :: Maybe [Asset], --
    userUpdateHandle :: Maybe Handle, --
    userUpdateLocale :: Maybe Locale, --
    userUpdateSsoId :: Maybe UserSSOId, --
    userUpdateManagedBy :: Maybe ManagedBy, --
    userUpdateSsoIdRemoved :: Bool --
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "userUpdate" UserUpdate

data UserIdentityRemove = UserIdentityRemove
  { uirId :: UserId,
    uirEmail :: Maybe Email,
    uirPhone :: Maybe Phone
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "uir" UserIdentityRemove
