{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store.Effect where

import Polysemy
import Wire.CLI.Backend.Client (ClientId)
import Wire.CLI.Backend.Connection (Connection)
import Wire.CLI.Backend.Conv (Conv)
import Wire.CLI.Backend.Credential (ServerCredential)
import Wire.CLI.Backend.Notification (NotificationId)

data Store m a where
  SaveCreds :: ServerCredential -> Store m ()
  GetCreds :: Store m (Maybe ServerCredential)
  SaveConvs :: [Conv] -> Store m ()
  GetConvs :: Store m (Maybe [Conv])
  SaveClientId :: ClientId -> Store m ()
  GetClientId :: Store m (Maybe ClientId)
  SaveLastNotificationId :: NotificationId -> Store m ()
  GetLastNotificationId :: Store m (Maybe NotificationId)
  SaveConnections :: [Connection] -> Store m ()
  GetConnections :: Store m [Connection]
  AddConnection :: Connection -> Store m ()

makeSem ''Store
