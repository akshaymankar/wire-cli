module Wire.CLI.Store.File where

import qualified Data.Aeson as Aeson
import Polysemy
import Wire.CLI.Store.Effect

run :: Member (Embed IO) r => FilePath -> Sem (Store ': r) a -> Sem r a
run baseDir = interpret $
  embed . \case
    GetCreds -> getFrom baseDir credFile
    GetConvs -> getFrom baseDir convFile
    GetClientId -> getFrom baseDir clientIdFile
    GetLastNotificationId -> getFrom baseDir lastNotificationIdFile
    SaveCreds cred -> saveTo baseDir credFile cred
    SaveConvs convs -> saveTo baseDir convFile convs
    SaveClientId clientId -> saveTo baseDir clientIdFile clientId
    SaveLastNotificationId nid -> saveTo baseDir lastNotificationIdFile nid

credFile, convFile, clientIdFile, lastNotificationIdFile :: FilePath
credFile = "credential.json"
convFile = "conversations.json"
clientIdFile = "client-id.json"
lastNotificationIdFile = "last-notification-id.json"

saveTo :: Aeson.ToJSON a => FilePath -> FilePath -> a -> IO ()
saveTo baseDir f = Aeson.encodeFile (baseDir <> "/" <> f)

getFrom :: Aeson.FromJSON a => FilePath -> FilePath -> IO (Maybe a)
getFrom baseDir f = Aeson.decodeFileStrict (baseDir <> "/" <> f)
