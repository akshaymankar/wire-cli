{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store.Effect
  ( Store (..),
    saveCreds,
  )
where

import Polysemy
import Wire.CLI.Backend.Credential

data Store m a where
  SaveCreds :: ServerCredential -> Store m ()

makeSem ''Store
