{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Wire.GUI.Conversation where

import Control.Concurrent.Chan.Unagi (InChan)
import Control.Monad (void, (>=>))
import qualified Data.GI.Gio.ListModel.CustomStore as Gio
import qualified Data.GI.Gio.ListModel.SeqStore as Gio
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GI.Gtk (AttrOp ((:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.CLI.Backend (Backend, Conv)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.CommonTypes (unwrapName)
import qualified Wire.CLI.Backend.Conv as Conv
import qualified Wire.CLI.Backend.User as User
import Wire.CLI.Error (WireCLIError)
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.GUI.Wait (queueActionWithWaitLoopSimple)
import Wire.GUI.Worker (Work)

mkConvBox :: InChan Work -> IO Gtk.Box
mkConvBox workChan = do
  mainBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal,
        #spacing := 10,
        #marginTop := 10,
        #marginBottom := 10,
        #marginStart := 10,
        #marginEnd := 10,
        #baselinePosition := Gtk.BaselinePositionTop
      ]
  convListBox <- mkConvListView workChan
  Gtk.boxAppend mainBox convListBox
  pure mainBox

mkConvListView :: InChan Work -> IO Gtk.ListView
mkConvListView workChan = do
  factory <- new Gtk.SignalListItemFactory []
  void $ on factory #setup createEmptyConvItem
  void $ on factory #bind (populateConvItem workChan)

  model <- Gio.seqStoreNew []
  queueActionWithWaitLoopSimple workChan (fromMaybe [] <$> Store.getConvs) (populateModel model)

  selection <- new Gtk.SingleSelection [#model := model]
  new
    Gtk.ListView
    [ #model := selection,
      #factory := factory
    ]

populateModel :: Gio.SeqStore Conv -> Either WireCLIError [Conv] -> IO ()
populateModel _ (Left err) = print err
populateModel model (Right convs) = do
  putStrLn "populate model"
  Gio.replaceList model convs

createEmptyConvItem :: Gtk.ListItem -> IO ()
createEmptyConvItem convListItem = do
  label <- new Gtk.Label [#label := "placeholder"]
  set convListItem [#child := label]

logAndThrow :: Member (Embed IO) r => Sem (Error String ': r) () -> Sem r ()
logAndThrow =
  Error.runError
    >=> either
      ( \err -> do
          embed . putStrLn $ err
          error $ "Unexpected error: " <> show err
      )
      pure

populateConvItem :: InChan Work -> Gtk.ListItem -> IO ()
populateConvItem workChan convListItem = runM . logAndThrow $ do
  embed $ putStrLn "poplate conv item"
  item <-
    get convListItem #item
      >>= Error.note "No item set: The conv ListItem contains no item!"

  storeItem <-
    embed (Gtk.castTo Gio.CustomStoreItem item)
      >>= Error.note "Expected StringObject: the conv ListItem is not a StringObject"
  conv <- Gio.deRefCustomStoreItem storeItem

  child <-
    get convListItem #child
      >>= Error.note "No child found: listConvItem"
  label <-
    embed (Gtk.castTo Gtk.Label child)
      >>= Error.note "Type cast failed: The child of conv ListItem is not a label!"

  embed $
    queueActionWithWaitLoopSimple workChan (convName conv) $ \e -> runM . logAndThrow $ do
      name <-
        Error.mapError (("Failed to get conv name: " <>) . show) $
          Error.fromEither e
      set label [#label := name]

convName :: Members '[Backend, Store, Error WireCLIError] r => Conv -> Sem r Text
convName conv =
  case Conv.convName conv of
    Just n -> pure n
    Nothing ->
      case Conv.convMembersOthers (Conv.convMembers conv) of
        [] ->
          -- An unnamed self converastion is called notes, this is usually not
          -- visible in other clients, maybe there need to be more checks that
          -- this is indeed the "self" conversation.
          pure "Notes"
        [othMem] -> do
          cred <- Store.getCredsOrErr
          -- TODO: Get this from store and only call backend when the user is
          -- not in store.
          unwrapName . User.userName <$> Backend.getUser cred (Conv.otherMemberId othMem)
        _ -> pure "Unnamed Conversation"
