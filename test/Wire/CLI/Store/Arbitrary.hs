{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.CLI.Store.Arbitrary where

import Data.ProtoLens.Arbitrary
import Data.ProtoLens.Labels ()
import Lens.Family2
import Proto.Messages
import Test.QuickCheck
import Wire.CLI.Backend.Arbitrary
import Wire.CLI.Store.StoredMessage

deriving via (ArbitraryMessage GenericMessage) instance Arbitrary GenericMessage

deriving via (GenericUniform StoredMessageData) instance Arbitrary StoredMessageData

deriving via (GenericUniform StoredMessage) instance Arbitrary StoredMessage

-- TODO: Make this not recursive
instance Arbitrary GenericMessage'Content where
  arbitrary = do
    msg :: GenericMessage <- arbitrary
    maybe arbitrary pure (view #maybe'content msg)
