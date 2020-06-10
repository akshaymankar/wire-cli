{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.CommonTypes where

import Data.Aeson (FromJSON)
import Data.Text (Text)

newtype Name = Name Text
  deriving (Show, Eq, FromJSON)
