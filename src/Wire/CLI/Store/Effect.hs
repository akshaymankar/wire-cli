module Wire.CLI.Store.Effect where

import Control.Algebra
import Data.Kind (Type)
import Wire.CLI.Backend.Credential

data Store (m :: Type -> Type) a where
  SaveCreds :: Credential -> Store m ()

saveCreds :: Has Store sig m => Credential -> m ()
saveCreds = send . SaveCreds
