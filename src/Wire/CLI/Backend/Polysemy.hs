{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend.Polysemy where

import Polysemy
import Wire.CLI.Backend.Types
import Wire.CLI.Options

data Backend m a where
  Login :: LoginOptions -> Backend m LoginResponse

makeSem ''Backend
