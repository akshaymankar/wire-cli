{-# LANGUAGE OverloadedLabels #-}

module Wire.GUI.App where

import Control.Concurrent.Chan.Unagi (InChan)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Data.Text (Text)
import qualified Data.Text as Text
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
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Store as Store
import Wire.GUI.Login (mkLoginBox)
import Wire.GUI.SlowSync (mkSlowSyncBox)
import Wire.GUI.Worker (Work, worker)
import qualified Wire.GUI.Worker as Worker

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
  eIsLoggedIn <- Worker.runActionSync workChan Store.isLoggedIn
  case eIsLoggedIn of
    Left err -> do
      errBox <- unrecoverableErrorBox err
      set window [#child := errBox]
    Right True -> afterLogin window workChan
    Right False -> do
      loginBox <- mkLoginBox (afterLogin window workChan) workChan
      set window [#child := loginBox]
  Gtk.widgetShow window

afterLogin :: Gtk.ApplicationWindow -> InChan Work -> IO ()
afterLogin window workChan = do
  b <- mkSlowSyncBox (afterSlowSync window) workChan
  set window [#child := b]

afterSlowSync :: Gtk.ApplicationWindow -> IO ()
afterSlowSync window = do
  b <- notImplementedBox "Flow after slow sync not implemented"
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

-- TODO: Maybe give user some ideas (buttons?) on how to recover like 'backup
-- convs -> reset the client -> import backup'
unrecoverableErrorBox :: WireCLIError -> IO Gtk.Box
unrecoverableErrorBox err = do
  b <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #valign := Gtk.AlignCenter,
        #baselinePosition := Gtk.BaselinePositionCenter
      ]
  msgLabel <- new Gtk.Label [#label := "An unrecoverable error happened!"]
  errLabel <- new Gtk.Label [#label := Text.pack (show err)]
  Gtk.boxAppend b msgLabel
  Gtk.boxAppend b errLabel
  pure b
