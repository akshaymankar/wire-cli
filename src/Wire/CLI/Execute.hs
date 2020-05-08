module Wire.CLI.Execute where

import Control.Monad ((<=<))
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

execute :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Opts.Command -> Sem r ()
execute = \case
  Opts.Login loginOpts -> performLogin loginOpts
  Opts.Logout -> error "Not implemented"

performLogin :: Members '[Backend, Store, CryptoBox, Error WireCLIError] r => Opts.LoginOptions -> Sem r ()
performLogin opts = do
  res <- Backend.login opts
  case res of
    Backend.LoginFailure e -> error $ show e
    Backend.LoginSuccess t -> do
      Store.saveCreds t
      preKeys <- mapM (throwCBoxError <=< CryptoBox.newPrekey) [0 .. 99]
      lastKey <- throwCBoxError =<< CryptoBox.newPrekey maxBound
      let client = Backend.NewClient "wire-cli-cookie-label" lastKey (Opts.loginPassword opts) "wire-cli" Backend.Permanent preKeys Backend.Desktop "wire-cli"
      Backend.registerClient t (Opts.loginServer opts) client
  pure ()

throwCBoxError :: (Member (Error WireCLIError) r) => CBox.Result a -> Sem r a
throwCBoxError res =
  case CryptoBox.resultToEither res of
    Left e -> Error.throw $ WireCLIError.UnexpectedCryptoBoxError e
    Right a -> pure a
