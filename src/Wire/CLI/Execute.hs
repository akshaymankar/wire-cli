module Wire.CLI.Execute where

import Data.Text
import Polysemy
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Options
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

execute :: Members '[Backend, Store] r => Command -> Sem r ()
execute = \case
  Login loginOpts -> performLogin loginOpts
  Logout -> error "Not implemented"

performLogin :: Members '[Backend, Store] r => LoginOptions -> Sem r ()
performLogin opts = do
  res <- Backend.login opts
  case res of
    Backend.LoginSuccess t -> Store.saveCreds t
    Backend.LoginFailure e -> error "Failed"
  pure ()
