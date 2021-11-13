{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Store.Effect where

import Data.Id (ClientId, ConvId)
import Data.Maybe (isJust)
import Data.Qualified
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.API.Connection (UserConnection)
import Wire.API.Conversation (Conversation)
import Wire.API.User (SelfProfile)
import Wire.CLI.Backend.Credential (ServerCredential)
import Wire.CLI.Backend.Notification (NotificationId)
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Store.StoredMessage (StoredMessage)

data Store m a where
  SaveCreds :: ServerCredential -> Store m ()
  GetCreds :: Store m (Maybe ServerCredential)
  SaveConvs :: [Conversation] -> Store m ()
  GetConvs :: Store m (Maybe [Conversation])
  SaveClientId :: ClientId -> Store m ()
  GetClientId :: Store m (Maybe ClientId)
  SaveLastNotificationId :: NotificationId -> Store m ()
  GetLastNotificationId :: Store m (Maybe NotificationId)
  SaveConnections :: [UserConnection] -> Store m ()
  GetConnections :: Store m [UserConnection]
  AddConnection :: UserConnection -> Store m ()
  AddMessage :: Qualified ConvId -> StoredMessage -> Store m ()
  GetLastNMessages :: Qualified ConvId -> Natural -> Store m [StoredMessage]
  SaveSelf :: SelfProfile -> Store m ()
  GetSelf :: Store m (Maybe SelfProfile)

makeSem ''Store

isLoggedIn :: Member Store r => Sem r Bool
isLoggedIn = isJust <$> getCreds

getCredsOrErr :: Members '[Store, Error WireCLIError] r => Sem r ServerCredential
getCredsOrErr = getCreds >>= Error.note WireCLIError.NotLoggedIn
