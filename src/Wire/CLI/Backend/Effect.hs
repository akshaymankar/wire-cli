{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend.Effect where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import Data.Handle
import Data.Id
import Data.Int
import Data.Qualified
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
  ListConvs :: ServerCredential -> Maybe (Range 1 500 Int32) -> Maybe ConvId -> Backend m (ConversationList Conversation)
  GetNotifications :: ServerCredential -> Natural -> ClientId -> NotificationId -> Backend m (NotificationGap, Notifications)
  RegisterWireless :: RegisterWirelessOptions -> Backend m [WireCookie]
  Register :: RegisterOptions -> Backend m [WireCookie]
  SetHandle :: ServerCredential -> Handle -> Backend m ()
  RequestActivationCode :: RequestActivationCodeOptions -> Backend m ()
  RefreshToken :: URI -> [WireCookie] -> Backend m Credential
  Search :: ServerCredential -> SearchOptions -> Backend m (SearchResult Contact)
  GetConnections :: ServerCredential -> Maybe (Range 1 500 Int32) -> Maybe ConnectionPagingState -> Backend m ConnectionsPage
  Connect :: ServerCredential -> Qualified UserId -> Backend m ()
  UpdateConnection :: ServerCredential -> Qualified UserId -> Relation -> Backend m ()
  GetPrekeyBundles :: ServerCredential -> QualifiedUserClients -> Backend m QualifiedUserClientPrekeyMap
  SendOtrMessage :: ServerCredential -> Qualified ConvId -> QualifiedNewOtrMessage -> Backend m (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus)
  GetUser :: ServerCredential -> Qualified UserId -> Backend m (Maybe UserProfile)
  GetSelf :: ServerCredential -> Backend m SelfProfile
  WatchNotifications :: ServerCredential -> Maybe ClientId -> Backend m (Unagi.OutChan WSNotification)

makeSem ''Backend
