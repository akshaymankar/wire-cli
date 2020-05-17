module Wire.CLI.Conv where

import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

sync :: Members '[Backend, Store, Error WireCLIError] r => Sem r ()
sync = do
  maybeCreds <- Store.getCreds
  case maybeCreds of
    Just creds -> do
      convs <- getAllConvs $ Backend.listConvs creds 500
      Store.saveConvs convs
    Nothing -> Error.throw WireCLIError.NotLoggedIn

getAllConvs :: Member Backend r => (Maybe Backend.ConvId -> Sem r Backend.Convs) -> Sem r [Backend.Conv]
getAllConvs f = loop Nothing
  where
    loop x = do
      (Backend.Convs convs hasMore) <- f x
      if hasMore
        then (convs <>) <$> loop (Just $ Backend.convId (last convs))
        else pure convs

list :: Members '[Store, Error WireCLIError] r => Sem r [Backend.Conv]
list = do
  maybeConvs <- Store.getConvs
  case maybeConvs of
    Just convs -> pure convs
    Nothing -> Error.throw WireCLIError.NoConversationsFound
