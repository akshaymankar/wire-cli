{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}

module Wire.GUI.SlowSync where

import Control.Concurrent.Chan.Unagi (InChan)
import qualified Data.Text as Text
import GI.Gtk (AttrOp ((:=)), new, set)
import qualified GI.Gtk as Gtk
import Polysemy
import Wire.CLI.Error (WireCLIError)
import Wire.GUI.Worker (Work)
import qualified Wire.GUI.Worker as Worker
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Execute (execute)

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
  Worker.queueAction workChan (slowSync msgLabel) (afterSyncAction done msgLabel)
  pure mainBox

afterSyncAction :: IO () -> Gtk.Label -> Either WireCLIError () -> IO ()
afterSyncAction done _ (Right _) = done
afterSyncAction _ msgLabel (Left err) =
  set msgLabel [#label := "Error occurred during sync: " <> Text.pack (show err)]

slowSync :: Members Worker.AllEffects r => Gtk.Label -> Sem r ()
slowSync msgLabel = do
  set msgLabel [#label := "Syncing conversations"]
  execute Opts.SyncConvs
  set msgLabel [#label := "Syncing connections"]
  execute Opts.SyncConnections
  set msgLabel [#label := "Syncing notifications"]
  execute Opts.SyncNotifications
  set msgLabel [#label := "Done syncing"]
