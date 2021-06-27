{-# LANGUAGE OverloadedLabels #-}

module Wire.GUI.App where

import Control.Concurrent.Chan.Unagi (InChan)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Data.Text (Text)
import qualified GI.Gio as Gio
import GI.Gtk (AttrOp ((:=)), new, set)
import qualified GI.Gtk as Gtk
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.OpenSSL as HTTP
import qualified OpenSSL.Session as SSL
import qualified OpenSSL.X509.SystemStore as SSL
import Polysemy
import Polysemy.Async (Async)
import qualified Polysemy.Async as Async
import qualified System.CryptoBox as CBox
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Wire.GUI.Login (mkLoginBox)
import Wire.GUI.Worker (Work, worker)

run :: IO ()
run = do
  mgr <- HTTP.newManager $ HTTP.opensslManagerSettings sslContext
  let storePath = "/tmp/optical-fiber"
  cbox <- openCBox $ cboxDir storePath
  runM . Async.asyncToIO $ guiAndWorker mgr storePath cbox

openCBox :: FilePath -> IO CBox.Box
openCBox dir = do
  Dir.createDirectoryIfMissing True dir
  CBox.open dir >>= \case
    CBox.Success b -> pure b
    err -> error $ "Failed to open crypto box: " ++ show err

cboxDir :: FilePath -> FilePath
cboxDir = (</> "cryptobox")

sslContext :: IO SSL.SSLContext
sslContext = do
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer True True Nothing
  SSL.contextLoadSystemCerts ctx
  pure ctx

guiAndWorker :: Members '[Embed IO, Async] r => HTTP.Manager -> FilePath -> CBox.Box -> Sem r ()
guiAndWorker mgr storePath cbox = do
  (uiChan, workerChan) <- embed Unagi.newChan
  let uiWork = embed $ gui uiChan
      backgroundWork = embed $ worker mgr storePath cbox workerChan
  maybeUnit <- Async.sequenceConcurrently [uiWork, backgroundWork]
  case sequence maybeUnit of
    Nothing -> pure ()
    Just _ -> error "Async failure"

gui :: InChan Work -> IO ()
gui workChan = do
  putStrLn "Started GUI!"
  app <- Gtk.applicationNew (Just "in.axeman.wire-gui") []
  _ <- Gio.onApplicationActivate app (appActivate app workChan)
  _ <- Gio.applicationRun app Nothing
  putStrLn "GUI dead"
  pure ()

appActivate :: Gtk.Application -> InChan Work -> Gio.ApplicationActivateCallback
appActivate app workChan = do
  window <-
    new
      Gtk.ApplicationWindow
      [ #title := "Optical Fiber",
        #application := app,
        #defaultWidth := 400,
        #defaultHeight := 300
      ]
  loginBox <- mkLoginBox (afterLogin window) workChan
  set window [#child := loginBox]
  Gtk.widgetShow window

afterLogin :: Gtk.ApplicationWindow -> IO ()
afterLogin window = do
  b <- notImplementedBox "Flow after login not implemented"
  set window [#child := b]

notImplementedBox :: Text -> IO Gtk.Box
notImplementedBox msg = do
  b <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #valign := Gtk.AlignCenter,
        #baselinePosition := Gtk.BaselinePositionCenter
      ]
  msgLabel <- new Gtk.Label [#label := msg]
  Gtk.boxAppend b msgLabel
  pure b