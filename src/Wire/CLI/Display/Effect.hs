{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Display.Effect where

import Polysemy
import Wire.CLI.Backend.Connection (Connection)
import Wire.CLI.Backend.Conv (Conv)
import Wire.CLI.Backend.Search (SearchResults)

data Display m a where
  ListConvs :: [Conv] -> Display m ()
  Search :: SearchResults -> Display m ()
  ListConnections :: [Connection] -> Display m ()

makeSem ''Display
