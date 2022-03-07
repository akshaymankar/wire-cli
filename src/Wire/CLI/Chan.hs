{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Chan where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Exception
import Polysemy

data ChanDead = ChanDead

data ReadChan m a where
  ReadChan :: Unagi.OutChan a -> ReadChan m (Either ChanDead a)

makeSem ''ReadChan

runRead :: Member (Embed IO) r => Sem (ReadChan ': r) a -> Sem r a
runRead = interpret $ \case
  ReadChan chan ->
    embed $
      (Right <$> Unagi.readChan chan)
        `catch` (\BlockedIndefinitelyOnMVar -> pure $ Left ChanDead)

data WriteChan m a where
  WriteChan :: Unagi.InChan a -> a -> WriteChan m ()

makeSem ''WriteChan

runWrite :: Member (Embed IO) r => Sem (WriteChan ': r) a -> Sem r a
runWrite = interpret $ \case
  WriteChan chan x -> embed $ Unagi.writeChan chan x

data NewChan m a where
  NewChan :: NewChan m (Unagi.InChan a, Unagi.OutChan a)

makeSem ''NewChan

runNew :: Member (Embed IO) r => Sem (NewChan ': r) a -> Sem r a
runNew = interpret $ \case
  NewChan -> embed Unagi.newChan
