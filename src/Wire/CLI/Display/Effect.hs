{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Display.Effect where

import Polysemy
import Wire.CLI.Store (StoredMessage)
import Data.Text (Text)
import Wire.API.Conversation (Conversation)
import Wire.API.Connection (UserConnection)
import Wire.API.User (SelfProfile)
import Wire.API.User.Search

data Display m a where
  ListConvs :: [Conversation] -> Display m ()
  Search :: SearchResult Contact -> Display m ()
  ListConnections :: [UserConnection] -> Display m ()
  ListMessages :: [StoredMessage] -> Display m ()
  Login :: Maybe Text -> Display m ()
  ShowSelfUser :: SelfProfile -> Display m ()

makeSem ''Display
