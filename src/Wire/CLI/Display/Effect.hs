{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Display.Effect where

import Polysemy
import Wire.CLI.Store (StoredMessage)
import Data.Text (Text)
import Wire.API.Conversation (Conversation)
import Wire.API.Connection (UserConnection)
import Wire.API.User (SelfProfile)
import Wire.API.User.Search
import Wire.CLI.Notification.Types (ProcessedNotification)
import qualified Control.Concurrent.Chan.Unagi as Unagi

data Display m a where
  ListConvs :: [Conversation] -> Display m ()
  Search :: SearchResult Contact -> Display m ()
  ListConnections :: [UserConnection] -> Display m ()
  ListMessages :: [StoredMessage] -> Display m ()
  Login :: Maybe Text -> Display m ()
  ShowSelfUser :: SelfProfile -> Display m ()
  ShowNotifications :: [ProcessedNotification] -> Display m ()
  ShowNotificationsLive :: Unagi.OutChan ProcessedNotification -> Display m ()

makeSem ''Display
