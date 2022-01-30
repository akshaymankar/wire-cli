{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Backend.Orphans where

import qualified Wire.API.Event.Team as Team

deriving instance Enum Team.EventType

deriving instance Bounded Team.EventType
