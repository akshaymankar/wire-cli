{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.UUIDGen where

import Data.UUID (UUID)
import qualified Data.UUID.V1 as UUIDv1
import qualified Data.UUID.V4 as UUIDv4
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)

data UUIDGen m a where
  GenV1 :: UUIDGen m (Maybe UUID)
  GenV4 :: UUIDGen m UUID

makeSem ''UUIDGen

run :: Member (Embed IO) r => Sem (UUIDGen ': r) a -> Sem r a
run =
  interpret $
    embed . \case
      GenV1 -> UUIDv1.nextUUID
      GenV4 -> UUIDv4.nextRandom
