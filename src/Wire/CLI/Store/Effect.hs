{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store.Effect where

import Polysemy
import Wire.CLI.Backend.Conv (Conv)
import Wire.CLI.Backend.Credential (ServerCredential)

data Store m a where
  SaveCreds :: ServerCredential -> Store m ()
  GetCreds :: Store m (Maybe ServerCredential)
  SaveConvs :: [Conv] -> Store m ()

makeSem ''Store
