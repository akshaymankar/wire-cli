module Wire.CLI.Connection where

import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.API.Connection
import Wire.API.Routes.MultiTablePaging
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Options (ListConnsOptions (..), UpdateConnOptions (..))
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

sync :: Members '[Backend, Store, Error WireCLIError] r => Sem r ()
sync = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  connList <- fetchAllConnections $ Backend.getConnections serverCreds Nothing
  Store.saveConnections connList

fetchAllConnections :: forall r. Member Backend r => (Maybe ConnectionPagingState -> Sem r ConnectionsPage) -> Sem r [UserConnection]
fetchAllConnections f = loop Nothing
  where
    loop :: Maybe ConnectionPagingState -> Sem r [UserConnection]
    loop x = do
      (MultiTablePage conns hasMore state) <- f x
      if hasMore
        then (conns <>) <$> loop (Just $ hackState state)
        else pure conns
    -- This is required because 'ConnectionPagingState' and
    -- 'ListConnectionsRequestPaginated' have different symbols to describe what
    -- they are. TODO: fix this in wire-api.
    hackState :: MultiTablePagingState n1 t -> MultiTablePagingState n2 t
    hackState (MultiTablePagingState tables state) = MultiTablePagingState tables state

list :: Members '[Store] r => ListConnsOptions -> Sem r [UserConnection]
list (ListConnsOptions relFilter) = do
  conns <- Store.getConnections
  case relFilter of
    Nothing -> pure conns
    Just rel -> pure $ filter (\c -> ucStatus c == rel) conns

update :: Members '[Backend, Store, Error WireCLIError] r => UpdateConnOptions -> Sem r ()
update (UpdateConnOptions user rel) = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  Backend.updateConnection serverCreds user rel
