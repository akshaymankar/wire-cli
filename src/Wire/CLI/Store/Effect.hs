{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store.Effect where

import Polysemy
import Wire.CLI.Backend.Client (ClientId)
import Wire.CLI.Backend.Conv (Conv)
import Wire.CLI.Backend.Credential (ServerCredential)

data Store m a where
  SaveCreds :: ServerCredential -> Store m ()
  GetCreds :: Store m (Maybe ServerCredential)
  SaveConvs :: [Conv] -> Store m ()
  GetConvs :: Store m (Maybe [Conv])
  SaveClientId :: ClientId -> Store m ()
  GetClientId :: Store m (Maybe ClientId)

makeSem ''Store
