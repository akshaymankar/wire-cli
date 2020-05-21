{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend.Effect where

import Numeric.Natural
import Polysemy
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Notification
import Wire.CLI.Options

data Backend m a where
  Login :: LoginOptions -> Backend m LoginResponse
  RegisterClient :: ServerCredential -> NewClient -> Backend m Client
  ListConvs :: ServerCredential -> Natural -> Maybe ConvId -> Backend m Convs
  GetNotifications :: ServerCredential -> Natural -> ClientId -> NotificationId -> Backend m (NotificationGap, Notifications)

makeSem ''Backend
