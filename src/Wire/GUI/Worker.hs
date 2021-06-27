module Wire.GUI.Worker where

import Control.Concurrent.Chan.Unagi (OutChan)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Monad ((<=<))
import qualified Network.HTTP.Client as HTTP
import Polysemy
import qualified Polysemy.Error as Error
import qualified Polysemy.Random as Random
import qualified System.CryptoBox as CBox
import qualified Wire.CLI.Backend.HTTP as HTTPBackend
import qualified Wire.CLI.CryptoBox.FFI as CryptoBoxFFI
import qualified Wire.CLI.Display.Print as PrintDisplay
import Wire.CLI.Error (WireCLIError)
import Wire.CLI.Execute (execute)
import qualified Wire.CLI.Options as Opts
import qualified Wire.CLI.Store.File as FileStore
import qualified Wire.CLI.UUIDGen as UUIDGen

-- TODO: Maybe the error should be 'SomeException'
data Work where
  Work :: Opts.Command a -> (Either WireCLIError a -> IO ()) -> Work

-- TODO: Wrap Unagi Chan stuff in its own effects
worker :: HTTP.Manager -> FilePath -> CBox.Box -> OutChan Work -> IO ()
worker mgr storePath cbox workChan = go
  where
    go :: IO ()
    go = do
      putStrLn "Waiting For work!"
      Work cmd callback <- Unagi.readChan workChan
      putStrLn "Got work!"
      callback
        <=< runFinal
          . embedToFinal
          . Error.runError -- TODO: log the error!
          . UUIDGen.run
          . Random.runRandomIO
          . PrintDisplay.run
          . HTTPBackend.run "wire-cli-label" mgr
          . FileStore.run storePath
          . CryptoBoxFFI.run cbox
          . execute
        $ cmd
      go
