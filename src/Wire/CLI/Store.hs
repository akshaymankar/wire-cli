{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store
  ( Store (..),
    saveCreds,
  )
where

import Data.Text
import Polysemy

data Store m a where
  SaveCreds :: Text -> Store m ()

makeSem ''Store
