{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Display.Effect where

import Polysemy
import Wire.CLI.Backend.Conv (Conv)

data Display m a where
  ListConvs :: [Conv] -> Display m ()

makeSem ''Display
