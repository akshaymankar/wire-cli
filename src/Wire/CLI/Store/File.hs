module Wire.CLI.Store.File where

import qualified Data.Aeson as Aeson
import Polysemy
import qualified Wire.CLI.Backend.Types as Backend
import Wire.CLI.Store.Polysemy

run :: Member (Embed IO) r => FilePath -> Sem (Store ': r) a -> Sem r a
run baseDir = interpret $ \case
  SaveCreds cred -> embed $ saveCredsToFile baseDir cred

saveCredsToFile :: FilePath -> Backend.Credential -> IO ()
saveCredsToFile baseDir = Aeson.encodeFile (baseDir <> "/credential.json")
