{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend.Effect where

import Network.URI (URI)
import Numeric.Natural
import Polysemy
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Connection
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Message
import Wire.CLI.Backend.Notification
import Wire.CLI.Backend.Search
import Wire.CLI.Backend.User
import Wire.CLI.Options

data Backend m a where
  Login :: LoginOptions -> Backend m LoginResponse
  RegisterClient :: ServerCredential -> NewClient -> Backend m Client
  ListConvs :: ServerCredential -> Natural -> Maybe ConvId -> Backend m Convs
  GetNotifications :: ServerCredential -> Natural -> ClientId -> NotificationId -> Backend m (NotificationGap, Notifications)
  RegisterWireless :: RegisterWirelessOptions -> Backend m [WireCookie]
  Register :: RegisterOptions -> Backend m [WireCookie]
  SetHandle :: ServerCredential -> Handle -> Backend m ()
  RequestActivationCode :: RequestActivationCodeOptions -> Backend m ()
  RefreshToken :: URI -> [WireCookie] -> Backend m Credential
  Search :: ServerCredential -> SearchOptions -> Backend m SearchResults
  GetConnections :: ServerCredential -> Natural -> Maybe UserId -> Backend m ConnectionList
  Connect :: ServerCredential -> ConnectionRequest -> Backend m ()
  UpdateConnection :: ServerCredential -> UserId -> Relation -> Backend m ()
  GetPrekeyBundles :: ServerCredential -> UserClients -> Backend m PrekeyBundles
  SendOtrMessage :: ServerCredential -> ConvId -> NewOtrMessage -> Backend m SendOtrMessageResponse
  GetUser :: ServerCredential -> UserId -> Backend m User
  -- | TODO: The backend differentiates between 'SelfUser' and 'User', client should too?
  GetSelf :: ServerCredential -> Backend m SelfUser

makeSem ''Backend
