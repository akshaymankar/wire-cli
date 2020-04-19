module Wire.CLI.Execute where

import Control.Algebra
import Control.Monad ((<=<))
import qualified System.CryptoBox as CBox
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Effect (Backend)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.CryptoBox.Effect (CryptoBox)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

execute :: (Has Backend sig m, Has Store sig m, Has CryptoBox sig m) => Opts.Command -> m ()
execute = \case
  Opts.Login loginOpts -> performLogin loginOpts
  Opts.Logout -> error "Not implemented"

performLogin :: (Has Backend sig m, Has Store sig m, Has CryptoBox sig m) => Opts.LoginOptions -> m ()
performLogin opts = do
  res <- Backend.login opts
  case res of
    Backend.LoginFailure e -> error $ show e
    Backend.LoginSuccess t -> do
      Store.saveCreds t
      preKeys <- mapM (ignoreCBoxResult <=< CryptoBox.newPrekey) [0 .. 99]
      lastKey <- ignoreCBoxResult =<< CryptoBox.newPrekey maxBound
      let client = Backend.NewClient "wire-cli-cookie-label" lastKey (Opts.loginPassword opts) "wire-cli" Backend.Permanent preKeys Backend.Desktop "wire-cli"
      Backend.registerClient t (Opts.loginServer opts) client
  pure ()

ignoreCBoxResult :: (Has CryptoBox sig m, Show a) => CBox.Result a -> m a
ignoreCBoxResult = \case
  CBox.Success a -> pure a
  err -> error $ "CryptoBox error: " <> show err
