module Wire.CLI.Store.File where

import Control.Monad (when)
import qualified Data.Aeson as Aeson
import Data.Domain
import Data.Id (ConvId)
import qualified Data.List as List
import Data.List.Extra (takeEnd)
import Data.Qualified
import Numeric.Natural (Natural)
import Polysemy
import System.Directory (doesFileExist)
import Wire.API.Connection (UserConnection)
import qualified Wire.API.Connection as Connection
import Wire.CLI.Store.Effect (Store (..))
import Wire.CLI.Store.StoredMessage
import qualified Data.Text as Text

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

convFile :: Qualified ConvId -> FilePath
convFile (Qualified conv domain) = "conv-" <> show conv <> "@" <> Text.unpack (domainText domain) <> ".json"

saveTo :: Aeson.ToJSON a => FilePath -> FilePath -> a -> IO ()
saveTo baseDir f = Aeson.encodeFile (baseDir <> "/" <> f)

getFrom :: Aeson.FromJSON a => FilePath -> FilePath -> IO (Maybe a)
getFrom baseDir f = do
  let file = baseDir <> "/" <> f
  fileExists <- doesFileExist file
  if fileExists
    then Aeson.decodeFileStrict file
    else pure Nothing

addConn :: FilePath -> UserConnection -> IO ()
addConn baseDir conn = do
  savedConns <- concat <$> getFrom baseDir connectionsFile
  let currentConvId = Connection.ucConvId conn
  case List.find (\a -> Connection.ucConvId a == currentConvId) savedConns of
    Nothing -> saveTo baseDir connectionsFile (savedConns <> [conn])
    Just savedConn ->
      when (Connection.ucLastUpdate savedConn < Connection.ucLastUpdate conn) $ do
        let savedConnsExceptCurrent = filter (\a -> Connection.ucConvId a /= currentConvId) savedConns
        saveTo baseDir connectionsFile (savedConnsExceptCurrent <> [conn])

-- | Maybe this will be too slow for long conversations
addMsg :: FilePath -> Qualified ConvId -> StoredMessage -> IO ()
addMsg baseDir conv newMsg = do
  savedMsgs <- concat <$> getFrom baseDir (convFile conv)
  saveTo baseDir (convFile conv) (savedMsgs <> [newMsg])

-- | Maybe this will be too slow for long conversations
getLastNMsgs :: FilePath -> Qualified ConvId -> Natural -> IO [StoredMessage]
getLastNMsgs baseDir conv n = do
  savedMsgs <- concat <$> getFrom baseDir (convFile conv)
  pure $ takeEnd (fromIntegral n) savedMsgs
