{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Chan where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import Polysemy

data ReadChan m a where
  ReadChan :: Unagi.OutChan a -> ReadChan m a

makeSem ''ReadChan

runRead :: Member (Embed IO) r => Sem (ReadChan ': r) a -> Sem r a
runRead = interpret $ \case
  ReadChan chan -> embed $ Unagi.readChan chan
