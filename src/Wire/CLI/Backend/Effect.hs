module Wire.CLI.Backend.Effect where

import Control.Algebra
import Data.Kind (Type)
import Network.URI (URI)
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.Credential
import qualified Wire.CLI.Options as Opts

data Backend (m :: Type -> Type) k where
  Login :: Opts.LoginOptions -> Backend m LoginResponse
  RegisterClient :: Credential -> URI -> NewClient -> Backend m ()

login :: Has Backend sig m => Opts.LoginOptions -> m LoginResponse
login = send . Login

registerClient :: Has Backend sig m => Credential -> URI -> NewClient -> m ()
registerClient c u n = send $ RegisterClient c u n
