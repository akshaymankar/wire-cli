{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend.Effect where

import Data.Handle
import Data.Id
import Data.Int
import Data.Range
import Network.URI (URI)
import Numeric.Natural
import Polysemy
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Message
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Search
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Notification
import Wire.CLI.Options

data Backend m a where
  Login :: LoginOptions -> Backend m LoginResponse
  RegisterClient :: ServerCredential -> NewClient -> Backend m Client
  ListConvs :: ServerCredential -> Natural -> Maybe ConvId -> Backend m (ConversationList Conversation)
  GetNotifications :: ServerCredential -> Natural -> ClientId -> NotificationId -> Backend m (NotificationGap, Notifications)
  RegisterWireless :: RegisterWirelessOptions -> Backend m [WireCookie]
  Register :: RegisterOptions -> Backend m [WireCookie]
  SetHandle :: ServerCredential -> Handle -> Backend m ()
  RequestActivationCode :: RequestActivationCodeOptions -> Backend m ()
  RefreshToken :: URI -> [WireCookie] -> Backend m Credential
  Search :: ServerCredential -> SearchOptions -> Backend m (SearchResult Contact)
  GetConnections :: ServerCredential -> Maybe (Range 1 500 Int32) -> Maybe UserId -> Backend m UserConnectionList
  Connect :: ServerCredential -> ConnectionRequest -> Backend m ()
  UpdateConnection :: ServerCredential -> UserId -> Relation -> Backend m ()
  GetPrekeyBundles :: ServerCredential -> UserClients -> Backend m UserClientPrekeyMap
  SendOtrMessage :: ServerCredential -> ConvId -> NewOtrMessage -> Backend m (Either (MessageNotSent ClientMismatch) ClientMismatch)
  GetUser :: ServerCredential -> UserId -> Backend m UserProfile
  GetSelf :: ServerCredential -> Backend m SelfProfile

makeSem ''Backend
