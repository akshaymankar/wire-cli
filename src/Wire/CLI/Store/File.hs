module Wire.CLI.Store.File where

import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Polysemy
import System.Directory (doesFileExist)
import Wire.CLI.Backend.Connection (Connection)
import qualified Wire.CLI.Backend.Connection as Connection
import Wire.CLI.Store.Effect

run :: Member (Embed IO) r => FilePath -> Sem (Store ': r) a -> Sem r a
run baseDir =
  interpret $
    embed . \case
      GetCreds -> getFrom baseDir credFile
      GetConvs -> getFrom baseDir convFile
      GetClientId -> getFrom baseDir clientIdFile
      GetLastNotificationId -> getFrom baseDir lastNotificationIdFile
      GetConnections -> concat <$> getFrom baseDir connectionsFile
      SaveCreds cred -> saveTo baseDir credFile cred
      SaveConvs convs -> saveTo baseDir convFile convs
      SaveClientId clientId -> saveTo baseDir clientIdFile clientId
      SaveLastNotificationId nid -> saveTo baseDir lastNotificationIdFile nid
      SaveConnections conns -> saveTo baseDir connectionsFile conns
      AddConnection conn -> addConn baseDir conn

credFile, convFile, clientIdFile, lastNotificationIdFile, connectionsFile :: FilePath
credFile = "credential.json"
convFile = "conversations.json"
clientIdFile = "client-id.json"
lastNotificationIdFile = "last-notification-id.json"
connectionsFile = "connections.json"

saveTo :: Aeson.ToJSON a => FilePath -> FilePath -> a -> IO ()
saveTo baseDir f = Aeson.encodeFile (baseDir <> "/" <> f)

getFrom :: Aeson.FromJSON a => FilePath -> FilePath -> IO (Maybe a)
getFrom baseDir f = do
  let file = baseDir <> "/" <> f
  fileExists <- doesFileExist file
  if fileExists
    then Aeson.decodeFileStrict file
    else pure Nothing

addConn :: FilePath -> Connection -> IO ()
addConn baseDir conn = do
  savedConns <- concat <$> getFrom baseDir connectionsFile
  let currentConvId = Connection.connectionConversation conn
  case List.find (\a -> Connection.connectionConversation a == currentConvId) savedConns of
    Nothing -> saveTo baseDir connectionsFile (savedConns <> [conn])
    Just savedConn ->
      when (Connection.connectionLastUpdate savedConn < Connection.connectionLastUpdate conn) $ do
        let savedConnsExceptCurrent = filter (\a -> Connection.connectionConversation a /= currentConvId) savedConns
        saveTo baseDir connectionsFile (savedConnsExceptCurrent <> [conn])
