{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store.Polysemy
  ( Store (..),
    saveCreds,
  )
where

import Polysemy
import Wire.CLI.Backend.Types

data Store m a where
  SaveCreds :: Credential -> Store m ()

makeSem ''Store
