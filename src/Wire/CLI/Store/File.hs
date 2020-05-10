module Wire.CLI.Store.File where

import qualified Data.Aeson as Aeson
import Polysemy
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Store.Effect

run :: Member (Embed IO) r => FilePath -> Sem (Store ': r) a -> Sem r a
run baseDir = interpret $ \case
  SaveCreds cred -> embed $ saveCredsToFile baseDir cred
  GetCreds -> embed $ getCredsFromFile baseDir
  SaveConvs convs -> embed $ saveConvsToFile baseDir convs

saveCredsToFile :: FilePath -> Backend.ServerCredential -> IO ()
saveCredsToFile baseDir = Aeson.encodeFile (baseDir <> "/credential.json")

getCredsFromFile :: FilePath -> IO (Maybe Backend.ServerCredential)
getCredsFromFile baseDir = Aeson.decodeFileStrict (baseDir <> "/credential.json")

saveConvsToFile :: FilePath -> [Backend.Conv] -> IO ()
saveConvsToFile baseDir = Aeson.encodeFile (baseDir <> "/conversations.json")
