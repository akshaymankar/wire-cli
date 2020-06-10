{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Service where

import Data.Text (Text)
import Wire.CLI.Util.JSONStrategy

newtype ServiceId = ServiceId Text
  deriving (Show, Eq, FromJSON, ToJSON)

newtype ProviderId = ProviderId Text
  deriving (Show, Eq, FromJSON, ToJSON)

data Service = Service
  { serviceId :: ServiceId,
    serviceProvider :: ProviderId
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via JSONStrategy "service" Service
