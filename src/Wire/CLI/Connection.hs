module Wire.CLI.Connection where

import Data.Id (UserId)
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.API.Connection (UserConnection (ucStatus, ucTo), UserConnectionList (UserConnectionList))
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Options (ListConnsOptions (..), UpdateConnOptions (..))
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Data.Qualified

sync :: Members '[Backend, Store, Error WireCLIError] r => Sem r ()
sync = do
  serverCreds <-
    Store.getCreds
      >>= Error.note WireCLIError.NotLoggedIn
  connList <- fetchAllConnections $ Backend.getConnections serverCreds 500
  Store.saveConnections connList

fetchAllConnections :: Member Backend r => (Maybe UserId -> Sem r UserConnectionList) -> Sem r [UserConnection]
fetchAllConnections f = loop Nothing
  where
    loop x = do
      (UserConnectionList conns hasMore) <- f x
      if hasMore
        then (conns <>) <$> loop (Just . qUnqualified $ ucTo (last conns))
        else pure conns

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
