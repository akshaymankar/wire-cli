module Wire.CLI.Store.File where

import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Data.List.Extra (takeEnd)
import Numeric.Natural (Natural)
import Polysemy
import System.Directory (doesFileExist)
import Wire.CLI.Backend.Connection (Connection)
import qualified Wire.CLI.Backend.Connection as Connection
import Wire.CLI.Backend.Conv (ConvId (..))
import Wire.CLI.Store.Effect (Store (..))
import Wire.CLI.Store.StoredMessage

run :: Member (Embed IO) r => FilePath -> Sem (Store ': r) a -> Sem r a
run baseDir =
  interpret $
    embed . \case
      GetCreds -> getFrom baseDir credFile
      GetConvs -> getFrom baseDir convsFile
      GetClientId -> getFrom baseDir clientIdFile
      GetLastNotificationId -> getFrom baseDir lastNotificationIdFile
      GetConnections -> concat <$> getFrom baseDir connectionsFile
      SaveCreds cred -> saveTo baseDir credFile cred
      SaveConvs convs -> saveTo baseDir convsFile convs
      SaveClientId clientId -> saveTo baseDir clientIdFile clientId
      SaveLastNotificationId nid -> saveTo baseDir lastNotificationIdFile nid
      SaveConnections conns -> saveTo baseDir connectionsFile conns
      AddConnection conn -> addConn baseDir conn
      AddMessage conv msg -> addMsg baseDir conv msg
      GetLastNMessages conv n -> getLastNMsgs baseDir conv n
      SaveSelf u -> saveTo baseDir selfFile u
      GetSelf -> getFrom baseDir selfFile

credFile, convsFile, clientIdFile, lastNotificationIdFile, connectionsFile, selfFile :: FilePath
credFile = "credential.json"
convsFile = "conversations.json"
clientIdFile = "client-id.json"
lastNotificationIdFile = "last-notification-id.json"
connectionsFile = "connections.json"
selfFile = "self.json"

convFile :: ConvId -> FilePath
convFile conv = "conv-" <> show conv <> ".json"

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

-- | Maybe this will be too slow for long conversations
addMsg :: FilePath -> ConvId -> StoredMessage -> IO ()
addMsg baseDir conv newMsg = do
  savedMsgs <- concat <$> getFrom baseDir (convFile conv)
  saveTo baseDir (convFile conv) (savedMsgs <> [newMsg])

-- | Maybe this will be too slow for long conversations
getLastNMsgs :: FilePath -> ConvId -> Natural -> IO [StoredMessage]
getLastNMsgs baseDir conv n = do
  savedMsgs <- concat <$> getFrom baseDir (convFile conv)
  pure $ takeEnd (fromIntegral n) savedMsgs
