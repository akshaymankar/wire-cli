module Wire.CLI.Connection where

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
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  connList <- getAllConnections $ Backend.getConnections serverCreds 500
  Store.saveConnections connList

getAllConnections :: Member Backend r => (Maybe Backend.UserId -> Sem r Backend.ConnectionList) -> Sem r [Backend.Connection]
getAllConnections f = loop Nothing
  where
    loop x = do
      (Backend.ConnectionList conns hasMore) <- f x
      if hasMore
        then (conns <>) <$> loop (Just $ Backend.connectionTo (last conns))
        else pure conns
