module Wire.CLI.User
  ( getSelf,
    syncSelf,
    SelfUser (..),
  )
where

import Control.Monad.Extra (fromMaybeM)
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.User (SelfUser (..))
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

getSelf :: Members '[Store, Backend, Error WireCLIError] r => Opts.GetSelfOptions -> Sem r SelfUser
getSelf opts = do
  if Opts.getSelfForceRefresh opts
    then syncSelf
    else fromMaybeM syncSelf Store.getSelf

syncSelf :: Members '[Store, Backend, Error WireCLIError] r => Sem r SelfUser
syncSelf = do
  creds <- Store.getCreds >>= Error.note WireCLIError.NotLoggedIn
  self <- Backend.getSelf creds
  Store.saveSelf self
  pure self
