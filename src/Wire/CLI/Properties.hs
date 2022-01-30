{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Properties where

import qualified Data.Aeson as Aeson
import Data.Id (ConvId)
import Data.Schema
import Data.Text (Text)
import Wire.API.User (Name)
import Wire.CLI.Util.JSONStrategy

data ReadReciept
  = ReadRecieptEnabled
  | ReadRecieptDisabled
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Schema ReadReciept)

instance ToSchema ReadReciept where
  schema =
    enum @Integer "ReadReciept" $
      mconcat
        [ element 0 ReadRecieptDisabled,
          element 1 ReadRecieptEnabled
        ]

newtype FolderId = FolderId Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data FolderType
  = FolderTypeCustom
  | FolderTypeFavourites
  | FolderTypeUnknown
  deriving (Show, Eq, Generic)

instance ToSchema FolderType where
  schema =
    enum @Integer "FolderType" $
      mconcat
        [ element 0 FolderTypeCustom,
          element 1 FolderTypeFavourites
        ]

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
  deriving (ToJSON, FromJSON) via Schema Folder

instance ToSchema Folder where
  schema =
    object "Folder" $
      Folder
        <$> folderId .= field "id" schema
        <*> folderName .= field "name" schema
        <*> folderType .= field "type" schema
        <*> folderConversations .= field "conversations" (array schema)
