{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Properties where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Wire.CLI.Util.JSONStrategy
import Data.Id (ConvId)
import Wire.API.User (Name)

data ReadReciept
  = ReadRecieptEnabled
  | ReadRecieptDisabled
  deriving (Show, Eq, Generic)

instance FromJSON ReadReciept where
  parseJSON = Aeson.withScientific "ReadReciept" $ \case
    0 -> pure ReadRecieptDisabled
    _ -> pure ReadRecieptEnabled

newtype FolderId = FolderId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FolderType
  = FolderTypeCustom
  | FolderTypeFavourites
  | FolderTypeUnknown
  deriving (Show, Eq, Generic)

instance FromJSON FolderType where
  parseJSON = Aeson.withScientific "FolderType" $ \case
    0 -> pure FolderTypeCustom
    1 -> pure FolderTypeFavourites
    _ -> pure FolderTypeUnknown

data Folder = Folder
  { folderId :: FolderId,
    folderName :: Name,
    folderType :: FolderType,
    folderConversations :: [ConvId]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "folder" Folder
