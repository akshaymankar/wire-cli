{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.CLI.Store.Arbitrary where

import Test.QuickCheck
import Wire.CLI.Backend.Arbitrary
import Wire.CLI.Store.StoredMessage

deriving via (GenericUniform StoredMessage) instance Arbitrary StoredMessage
