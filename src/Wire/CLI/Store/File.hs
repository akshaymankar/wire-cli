{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Wire.CLI.Store.File where

import Control.Algebra
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Wire.CLI.Backend.Credential as Backend
import Wire.CLI.Store.Effect

run :: FilePath -> File m a -> m a
run baseDir = runReader baseDir . runFile

newtype File m a = File {runFile :: ReaderC FilePath m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Store :+: sig) (File m) where
  alg hdl sig ctx =
    File $
      case sig of
        L (SaveCreds cred) -> do
          baseDir <- ask
          (<$ ctx) <$> liftIO (saveCredsToFile baseDir cred)
        R other -> alg (runFile . hdl) (R other) ctx

saveCredsToFile :: FilePath -> Backend.Credential -> IO ()
saveCredsToFile baseDir = Aeson.encodeFile (baseDir <> "/credential.json")
