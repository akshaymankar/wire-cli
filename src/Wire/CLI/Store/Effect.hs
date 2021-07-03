{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store.Effect where

import Numeric.Natural (Natural)
import Polysemy
import Wire.CLI.Backend.Client (ClientId)
import Wire.CLI.Backend.Connection (Connection)
import Wire.CLI.Backend.Conv (Conv, ConvId)
import Wire.CLI.Backend.Credential (ServerCredential)
import Wire.CLI.Backend.Notification (NotificationId)
import Wire.CLI.Store.StoredMessage (StoredMessage)
import Data.Maybe (isJust)
import Polysemy.Error (Error)
import Wire.CLI.Error (WireCLIError)
import qualified Polysemy.Error as Error
import qualified Wire.CLI.Error as WireCLIError

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
  AddMessage :: ConvId -> StoredMessage -> Store m ()
  GetLastNMessages :: ConvId -> Natural -> Store m [StoredMessage]

makeSem ''Store

isLoggedIn :: Member Store r => Sem r Bool
isLoggedIn = isJust <$> getCreds

getCredsOrErr :: Members '[Store, Error WireCLIError] r => Sem r ServerCredential
getCredsOrErr = getCreds >>= Error.note WireCLIError.NotLoggedIn
