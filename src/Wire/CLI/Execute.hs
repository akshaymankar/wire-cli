module Wire.CLI.Execute where

import Polysemy
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

execute :: Members '[Backend, Store] r => Opts.Command -> Sem r ()
execute = \case
  Opts.Login loginOpts -> performLogin loginOpts
  Opts.Logout -> error "Not implemented"

performLogin :: Members '[Backend, Store] r => Opts.LoginOptions -> Sem r ()
performLogin opts = do
  res <- Backend.login opts
  case res of
    Backend.LoginSuccess t -> Store.saveCreds t
    Backend.LoginFailure _e -> error "Failed"
  pure ()
