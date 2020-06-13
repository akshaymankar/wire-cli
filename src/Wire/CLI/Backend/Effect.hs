{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend.Effect where

import Network.URI (URI)
import Numeric.Natural
import Polysemy
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.Credential
import Wire.CLI.Backend.Notification
import Wire.CLI.Backend.Search
import Wire.CLI.Options

data Backend m a where
  Login :: LoginOptions -> Backend m LoginResponse
  RegisterClient :: ServerCredential -> NewClient -> Backend m Client
  ListConvs :: ServerCredential -> Natural -> Maybe ConvId -> Backend m Convs
  GetNotifications :: ServerCredential -> Natural -> ClientId -> NotificationId -> Backend m (NotificationGap, Notifications)
  RegisterWireless :: RegisterWirelessOptions -> Backend m [WireCookie]
  RefreshToken :: URI -> [WireCookie] -> Backend m AccessToken
  Search :: ServerCredential -> SearchOptions -> Backend m SearchResults

makeSem ''Backend
