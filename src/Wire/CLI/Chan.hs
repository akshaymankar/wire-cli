{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Chan where

import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UnagiNB
import Control.Exception
import Polysemy

data ChanDead = ChanDead

data ReadChan m a where
  ReadChan :: UnagiNB.OutChan a -> ReadChan m (Either ChanDead a)

makeSem ''ReadChan

runRead :: Member (Embed IO) r => Sem (ReadChan ': r) a -> Sem r a
runRead = interpret $ \case
  ReadChan chan ->
    embed $
      (Right <$> UnagiNB.readChan mempty chan)
        `catch` (\BlockedIndefinitelyOnMVar -> pure $ Left ChanDead)

data WriteChan m a where
  WriteChan :: UnagiNB.InChan a -> a -> WriteChan m ()

makeSem ''WriteChan

runWrite :: Member (Embed IO) r => Sem (WriteChan ': r) a -> Sem r a
runWrite = interpret $ \case
  WriteChan chan x -> embed $ UnagiNB.writeChan chan x

data NewChan m a where
  NewChan :: NewChan m (UnagiNB.InChan a, UnagiNB.OutChan a)

makeSem ''NewChan

runNew :: Member (Embed IO) r => Sem (NewChan ': r) a -> Sem r a
runNew = interpret $ \case
  NewChan -> embed UnagiNB.newChan
