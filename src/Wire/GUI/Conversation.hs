{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Wire.GUI.Conversation where

import Control.Concurrent.Chan.Unagi (InChan)
import Control.Monad (void, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.GI.Gio.ListModel.CustomStoreItem as Gio
import qualified Data.GI.Gio.ListModel.SeqStore as Gio
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import GI.Gtk (AttrOp (On, (:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import Lens.Family2 (view)
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import qualified Proto.Messages as Message
import Wire.CLI.Backend (Backend, Conv)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.CommonTypes (unwrapName)
import qualified Wire.CLI.Backend.Conv as Conv
import qualified Wire.CLI.Backend.User as User
import Wire.CLI.Error (WireCLIError)
import Wire.CLI.Execute (execute)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store, StoredMessage)
import qualified Wire.CLI.Store as Store
import qualified Wire.CLI.Store.StoredMessage as StoredMessage
import Wire.GUI.Wait (queueActionWithWaitLoopSimple)
import Wire.GUI.Worker (Work)

mkConvBox :: InChan Work -> IO Gtk.Paned
mkConvBox workChan = do
  (messageBox, messageViewStore) <- mkMessageBox workChan
  convListBox <- mkConvListView workChan messageViewStore
  new
    Gtk.Paned
    [ #startChild := convListBox,
      #endChild := messageBox
    ]

mkConvListView :: InChan Work -> Gio.SeqStore StoredMessage -> IO Gtk.ListView
mkConvListView workChan messageViewStore = do
  factory <-
    new
      Gtk.SignalListItemFactory
      [ On #setup createEmptyConvItem,
        On #bind (populateConvItem workChan)
      ]

  model <- Gio.seqStoreFromList []
  queueActionWithWaitLoopSimple workChan (fromMaybe [] <$> Store.getConvs) (populateModel model)

  selection <- new Gtk.SingleSelection [#model := model]
  void $ on selection #selectionChanged (convSelected workChan messageViewStore model selection)
  new
    Gtk.ListView
    [ #model := selection,
      #factory := factory
    ]

mkMessageBox :: InChan Work -> IO (Gtk.ListView, Gio.SeqStore StoredMessage)
mkMessageBox workChan = do
  factory <-
    new
      Gtk.SignalListItemFactory
      [ On #setup createEmptyMessageItem,
        On #bind (populateMessageItem workChan)
      ]

  model <- Gio.seqStoreFromList []

  selection <- new Gtk.NoSelection [#model := model]
  messageView <-
    new
      Gtk.ListView
      [ #model := selection,
        #factory := factory
      ]

  pure (messageView, model)

createEmptyMessageItem :: Gtk.SignalListItemFactorySetupCallback
createEmptyMessageItem msgListItem = do
  box <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 10,
        #marginTop := 10,
        #marginBottom := 10,
        #marginStart := 10,
        #marginEnd := 10
      ]
  sender <- new Gtk.Label [#name := "sender"]
  message <- new Gtk.Label [#name := "message"]
  Gtk.boxAppend box sender
  Gtk.boxAppend box message
  set msgListItem [#child := box]

populateMessageItem :: InChan Work -> Gtk.SignalListItemFactoryBindCallback
populateMessageItem workChan msgListItem = runM . logAndThrowPolysemy $ do
  box <-
    listItemGetCastedChild msgListItem Gtk.Box
      >>= Error.mapError ("error while populating message item: " <>) . Error.fromEither

  sender <-
    widgetGetChildByName box "sender"
      >>= Error.note "failed to get sender"
      >>= embed . Gtk.castTo Gtk.Label
      >>= Error.note "failed to cast sender"

  messageLabel <-
    widgetGetChildByName box "message"
      >>= Error.note "failed to get message"
      >>= embed . Gtk.castTo Gtk.Label
      >>= Error.note "failed to cast message"
  item <-
    get msgListItem #item
      >>= Error.note "No item set: The ListItem contains no item!"

  msg <-
    Gio.fromObject item
      >>= Error.note "Expected StoredMessage: the ListItem is not a StoredMessage"

  liftIO . queueActionWithWaitLoopSimple workChan (getUserName (StoredMessage.smSenderUser msg)) $
    \e -> runM . logAndThrowPolysemy $ do
      username <- Error.mapError (("failed to get username" <>) . show) . Error.fromEither $ e
      set sender [#label := username]
  set messageLabel [#label := msgText msg]

msgText :: StoredMessage -> Text
msgText sm =
  case StoredMessage.smMessage sm of
    StoredMessage.InvalidMessage reason -> "Invalid message: " <> Text.pack reason
    StoredMessage.ValidMessage msg ->
      case view #maybe'content msg of
        Just (Message.GenericMessage'Text txt) -> view #content txt
        _ -> "Unhandled message"

getUserName :: Members '[Backend, Store, Error WireCLIError] r => User.UserId -> Sem r Text
getUserName uid = do
  cred <- Store.getCredsOrErr
  -- TODO: Get this from store and only call backend when the user is not in
  -- store.
  unwrapName . User.userName <$> Backend.getUser cred uid

listItemGetCastedChild :: (MonadIO m, Gtk.GObject child) => Gtk.ListItem -> (Gtk.ManagedPtr child -> child) -> m (Either String child)
listItemGetCastedChild listItem childCon = liftIO . runM . Error.runError $ do
  childWidget <-
    get listItem #child
      >>= Error.note "no child found"
  embed (Gtk.castTo childCon childWidget)
    >>= Error.note "type casting failed"

widgetGetChildByName :: (Gtk.IsWidget parent, MonadIO m) => parent -> Text -> m (Maybe Gtk.Widget)
widgetGetChildByName parent name = do
  firstChild <- Gtk.widgetGetFirstChild parent
  go firstChild
  where
    go Nothing = pure Nothing
    go (Just child) = do
      childName <- Gtk.widgetGetName child
      if childName == name
        then pure (Just child)
        else go =<< Gtk.widgetGetNextSibling child

populateModel :: Gio.SeqStore Conv -> Either WireCLIError [Conv] -> IO ()
populateModel _ (Left err) = print err
populateModel model (Right convs) = do
  putStrLn "populate model"
  Gio.replaceList model convs

createEmptyConvItem :: Gtk.ListItem -> IO ()
createEmptyConvItem convListItem = do
  label <- new Gtk.Label []
  set convListItem [#child := label]

logAndThrowPolysemy :: Member (Embed IO) r => Sem (Error String ': r) a -> Sem r a
logAndThrowPolysemy =
  Error.runError
    >=> either
      ( \err -> do
          embed . putStrLn $ err
          error $ "Unexpected error: " <> show err
      )
      pure

logAndThrow :: MonadIO m => String -> m ()
logAndThrow err = liftIO $ do
  putStrLn err
  error $ "Unexpected error: " <> show err

populateConvItem :: InChan Work -> Gtk.ListItem -> IO ()
populateConvItem workChan convListItem = runM . logAndThrowPolysemy $ do
  item <-
    get convListItem #item
      >>= Error.note "No item set: The conv ListItem contains no item!"

  conv <-
    Gio.fromObject item
      >>= Error.note "Expected Conv: the conv ListItem is not a Conv"

  child <-
    get convListItem #child
      >>= Error.note "No child found: listConvItem"
  label <-
    embed (Gtk.castTo Gtk.Label child)
      >>= Error.note "Type cast failed: The child of conv ListItem is not a label!"

  embed $
    queueActionWithWaitLoopSimple workChan (convName conv) $
      \e -> runM . logAndThrowPolysemy $ do
        name <- fromEitherStringified "Failed to get conv name: " e
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
        [othMem] -> getUserName (Conv.otherMemberId othMem)
        _ -> pure "Unnamed Conversation"

convSelected :: InChan Work -> Gio.SeqStore StoredMessage -> Gio.SeqStore Conv -> Gtk.SingleSelection -> Word32 -> Word32 -> IO ()
convSelected workChan messageViewStore store selection _ _ = do
  selectedPos <- get selection #selected
  putStrLn $ "Conv selected: " <> show selectedPos
  Gio.seqStoreLookup store (fromIntegral selectedPos) >>= \case
    Nothing -> logAndThrow "Selected conv out of bounds"
    Just conv -> loadMessages workChan messageViewStore conv

loadMessages :: InChan Work -> Gio.SeqStore StoredMessage -> Conv -> IO ()
loadMessages workChan messageViewStore conv = do
  let getMessages = execute . Opts.ListMessages $ Opts.ListMessagesOptions (Conv.convId conv) 100
  queueActionWithWaitLoopSimple workChan getMessages $
    \e -> runM . logAndThrowPolysemy $ do
      messages <- fromEitherStringified "Failed to get messages: " e
      Gio.replaceList messageViewStore messages

fromEitherStringified :: Member (Error String) r => String -> Either WireCLIError a -> Sem r a
fromEitherStringified prefix = Error.mapError ((prefix <>) . show) . Error.fromEither
