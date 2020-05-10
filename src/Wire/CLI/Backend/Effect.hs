{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend.Effect where

import Numeric.Natural
import Polysemy
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.Credential
import Wire.CLI.Options

data Backend m a where
  Login :: LoginOptions -> Backend m LoginResponse
  RegisterClient :: ServerCredential -> NewClient -> Backend m ()
  ListConvs :: ServerCredential -> Natural -> Maybe ConvId -> Backend m Convs

makeSem ''Backend
