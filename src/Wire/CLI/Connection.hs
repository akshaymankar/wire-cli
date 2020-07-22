module Wire.CLI.Connection where

import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Options (ListConnsOptions (..))
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

sync :: Members '[Backend, Store, Error WireCLIError] r => Sem r ()
sync = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  connList <- fetchAllConnections $ Backend.getConnections serverCreds 500
  Store.saveConnections connList

fetchAllConnections :: Member Backend r => (Maybe Backend.UserId -> Sem r Backend.ConnectionList) -> Sem r [Backend.Connection]
fetchAllConnections f = loop Nothing
  where
    loop x = do
      (Backend.ConnectionList conns hasMore) <- f x
      if hasMore
        then (conns <>) <$> loop (Just $ Backend.connectionTo (last conns))
        else pure conns

list :: Members '[Store] r => ListConnsOptions -> Sem r [Backend.Connection]
list (ListConnsOptions relFilter) = do
  conns <- Store.getConnections
  case relFilter of
    Nothing -> pure conns
    Just rel -> pure $ filter (\c -> Backend.connectionStatus c == rel) conns
