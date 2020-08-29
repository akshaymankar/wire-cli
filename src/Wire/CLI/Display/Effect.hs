{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Display.Effect where

import Polysemy
import Wire.CLI.Backend.Connection (Connection)
import Wire.CLI.Backend.Conv (Conv)
import Wire.CLI.Backend.Search (SearchResults)
import Wire.CLI.Store (StoredMessage)

data Display m a where
  ListConvs :: [Conv] -> Display m ()
  Search :: SearchResults -> Display m ()
  ListConnections :: [Connection] -> Display m ()
  ListMessages :: [StoredMessage] -> Display m ()

makeSem ''Display
