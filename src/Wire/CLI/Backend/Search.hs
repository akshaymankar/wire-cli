{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Wire.CLI.Backend.Search where

import Wire.CLI.Backend.CommonTypes
import Wire.CLI.Backend.User
import Wire.CLI.Util.JSONStrategy

newtype SearchResults = SearchResults {searchResultsDocuments :: [SearchResult]}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONStrategy "searchResults" SearchResults)

data SearchResult = SearchResult
  { searchResultHandle :: Maybe Handle,
    searchResultName :: Name,
    searchResultId :: UserId
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONStrategy "searchResult" SearchResult)
