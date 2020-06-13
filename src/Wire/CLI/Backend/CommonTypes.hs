{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.CommonTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

newtype Name = Name Text
  deriving (Show, Eq, FromJSON, ToJSON)
