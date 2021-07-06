{-# LANGUAGE OverloadedLabels #-}

module Wire.GUI.SlowSync where

import Control.Concurrent.Chan.Unagi (InChan)
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UnagiNB
import Data.Text (Text)
import qualified Data.Text as Text
import GI.Gtk (AttrOp ((:=)), new, set)
import qualified GI.Gtk as Gtk
import Polysemy
import Wire.CLI.Error (WireCLIError)
import Wire.CLI.Execute (execute)
import qualified Wire.CLI.Options as Opts
import Wire.GUI.Wait (queueActionAndStartWaitLoop)
import Wire.GUI.Worker (Work)
import qualified Wire.GUI.Worker as Worker

-- | Does these things in order:
-- - sync self profile (not implemented)
-- - sync convs
-- - sync conns
-- - sync notifications
-- - callback
mkSlowSyncBox :: IO () -> InChan Work -> IO Gtk.Box
mkSlowSyncBox done workChan = do
  mainBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #valign := Gtk.AlignCenter,
        #baselinePosition := Gtk.BaselinePositionCenter
      ]
  msgLabel <- new Gtk.Label [#label := "Starting slow sync..."]
  Gtk.boxAppend mainBox msgLabel
  queueActionAndStartWaitLoop workChan slowSync slowSyncHandleErr (onUpdate msgLabel done)
  pure mainBox

data SlowSyncStatusUpdate
  = SlowSyncDone
  | SlowSyncError WireCLIError
  | SlowSyncMessage Text

onUpdate :: Gtk.Label -> IO () -> SlowSyncStatusUpdate -> IO Bool
onUpdate msgLabel done = \case
  SlowSyncMessage msg -> do
    putStrLn $ "Got message " <> show msg
    set msgLabel [#label := msg]
    pure True
  SlowSyncDone -> do
    set msgLabel [#label := "Done syncing"]
    done
    pure False
  SlowSyncError err -> do
    set msgLabel [#label := "Error occurred during sync: " <> Text.pack (show err)]
    pure False

slowSync :: Members Worker.AllEffects r => UnagiNB.InChan SlowSyncStatusUpdate -> Sem r ()
slowSync chan = do
  embed $ UnagiNB.writeChan chan $ SlowSyncMessage "Syncing conversations"
  execute Opts.SyncConvs
  embed $ UnagiNB.writeChan chan $ SlowSyncMessage "Syncing connections"
  execute Opts.SyncConnections
  embed $ UnagiNB.writeChan chan $ SlowSyncMessage "Syncing notifications"
  execute Opts.SyncNotifications
  embed $ UnagiNB.writeChan chan SlowSyncDone

slowSyncHandleErr :: UnagiNB.InChan SlowSyncStatusUpdate -> Either WireCLIError () -> IO ()
slowSyncHandleErr chan (Left err) = UnagiNB.writeChan chan $ SlowSyncError err
slowSyncHandleErr _ _ = putStrLn "Sync done without errors!"
