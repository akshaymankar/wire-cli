{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Wire.GUI.Conversation where

import Control.Concurrent.Chan.Unagi (InChan)
import Control.Monad (void, (<=<))
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
import Wire.GUI.Worker (Work)
import qualified Wire.GUI.Worker as Worker

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
  void $ on factory #bind populateConvItem

  model <- Gtk.stringListNew (Just [])
  Worker.queueAction workChan getConvsWithName (populateModel model)

  selection <- new Gtk.SingleSelection [#model := model]
  new
    Gtk.ListView
    [ #model := selection,
      #factory := factory
    ]

-- TODO: Use better model
populateModel :: Gtk.StringList -> Either WireCLIError [(Text, Conv)] -> IO ()
populateModel _ (Left err) = print err
populateModel model (Right convsWithName) = do
  let names = map fst convsWithName
  Gtk.stringListSplice model 0 0 (Just names)

createEmptyConvItem :: Gtk.ListItem -> IO ()
createEmptyConvItem convListItem = do
  label <- new Gtk.Label [#label := "placeholder"]
  set convListItem [#child := label]

logAndThrow :: Member (Embed IO) r => Sem (Error String ': r) () -> Sem r ()
logAndThrow = either (embed . putStrLn) pure <=< Error.runError

populateConvItem :: Gtk.ListItem -> IO ()
populateConvItem convListItem = runM . logAndThrow $ do
  item <-
    get convListItem #item
      >>= Error.note "No item set: The conv ListItem contains no item!"

  -- nameGVal <- embed $ GIBase.newGValue GType.gtypeString
  -- GObject.objectGetProperty item "name" nameGVal
  -- name <- embed $ fromMaybe "Unnamed" <$> Gtk.fromGValue @(Maybe Text) nameGVal
  str <-
    embed (Gtk.castTo Gtk.StringObject item)
      >>= Error.note "Expected StringObject: the conv ListItem is not a StringObject"
  name <- get str #string

  child <-
    get convListItem #child
      >>= Error.note "No child found: listConvItem"
  label <-
    embed (Gtk.castTo Gtk.Label child)
      >>= Error.note "Type cast failed: The child of conv ListItem is not a label!"

  set label [#label := name]

getConvsWithName :: Members '[Backend, Store, Error WireCLIError] r => Sem r [(Text, Conv)]
getConvsWithName = do
  convs <- fromMaybe [] <$> Store.getConvs
  traverse (\c -> (,c) <$> convName c) convs

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
