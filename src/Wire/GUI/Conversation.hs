{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Wire.GUI.Conversation where

import Control.Concurrent.Chan.Unagi (InChan)
import Control.Monad (void, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.GI.Gio.ListModel.CustomStoreItem as Gio
import qualified Data.GI.Gio.ListModel.SeqStore as Gio
import Data.Id (ConvId, UserId)
import Data.Maybe (fromMaybe)
import qualified Data.ProtoLens as Proto
import Data.Qualified
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import GI.Gtk (AttrOp (On, (:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import Lens.Family2 (view, (&), (.~))
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import qualified Proto.Messages as M
import qualified Proto.Messages as Message
import Wire.API.Conversation hiding (Member)
import Wire.API.User (Name (fromName), UserProfile (profileName))
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Error (WireCLIError)
import Wire.CLI.Execute (execute)
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store, StoredMessage)
import qualified Wire.CLI.Store as Store
import qualified Wire.CLI.Store.StoredMessage as StoredMessage
import Wire.GUI.Wait (queueActionWithWaitLoopSimple)
import Wire.GUI.Worker (Work)

{-# ANN module ("HLint: ignore Redundant $" :: String) #-}

mkConvBox :: InChan Work -> IO Gtk.Paned
mkConvBox workChan = do
  (messageBox, convNameLabel, messageViewStore, newMessageTextView, sendMessageButton) <- mkMessageBox workChan
  convListBox <- mkConvListView workChan convNameLabel messageViewStore newMessageTextView sendMessageButton
  new Gtk.Paned $
    [ #startChild := convListBox,
      #endChild := messageBox,
      #resizeStartChild := False,
      #resizeEndChild := True,
      #shrinkStartChild := False
    ]

mkConvListView :: InChan Work -> Gtk.Label -> Gio.SeqStore StoredMessage -> Gtk.TextView -> Gtk.Button -> IO Gtk.ScrolledWindow
mkConvListView workChan convNameLabel messageViewStore newMessageTextView sendMessageButton = do
  factory <-
    new Gtk.SignalListItemFactory $
      [ On #setup createEmptyConvItem,
        On #bind (populateConvItem workChan)
      ]

  model <- Gio.seqStoreFromList []

  selection <- new Gtk.SingleSelection [#model := model]
  let onSelection = convSelected workChan convNameLabel messageViewStore newMessageTextView sendMessageButton model selection
  void $ on selection #selectionChanged onSelection

  queueActionWithWaitLoopSimple workChan (fromMaybe [] <$> Store.getConvs) $ \res -> do
    populateConvListModel model res
    onSelection 0 0

  listView <-
    new Gtk.ListView $
      [ #model := selection,
        #factory := factory
      ]

  new Gtk.ScrolledWindow $
    [ #child := listView,
      #hscrollbarPolicy := Gtk.PolicyTypeNever
    ]

mkMessageBox :: InChan Work -> IO (Gtk.Box, Gtk.Label, Gio.SeqStore StoredMessage, Gtk.TextView, Gtk.Button)
mkMessageBox workChan = do
  convNameLabel <- new Gtk.Label []

  factory <-
    new Gtk.SignalListItemFactory $
      [ On #setup createEmptyMessageItem,
        On #bind (populateMessageItem workChan)
      ]

  model <- Gio.seqStoreFromList []
  selection <- new Gtk.NoSelection [#model := model]
  messageView <-
    new Gtk.ListView $
      [ #model := selection,
        #factory := factory,
        #vexpand := True
      ]
  messsageViewScrolledWindow <- new Gtk.ScrolledWindow [#child := messageView]

  (sendMessageBox, newMessageTextView, sendMessageButton) <- mkSendMessageBox

  box <-
    new Gtk.Box $
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 0,
        #marginTop := 0,
        #marginBottom := 10,
        #marginStart := 10,
        #marginEnd := 10
      ]
  Gtk.boxAppend box convNameLabel
  Gtk.boxAppend box messsageViewScrolledWindow
  Gtk.boxAppend box sendMessageBox

  pure (box, convNameLabel, model, newMessageTextView, sendMessageButton)

mkSendMessageBox :: IO (Gtk.Box, Gtk.TextView, Gtk.Button)
mkSendMessageBox = do
  newMessageTextView <- new Gtk.TextView [#hexpand := True]
  sendMessageButton <- new Gtk.Button [#label := "Send"]

  sendMessageBox <-
    new Gtk.Box $
      [ #orientation := Gtk.OrientationHorizontal,
        #spacing := 0,
        #marginTop := 10,
        #marginBottom := 0,
        #marginStart := 0,
        #marginEnd := 0
      ]
  Gtk.boxAppend sendMessageBox newMessageTextView
  Gtk.boxAppend sendMessageBox sendMessageButton
  pure (sendMessageBox, newMessageTextView, sendMessageButton)

onSendMessageClicked :: Qualified ConvId -> Gtk.TextView -> Gio.SeqStore StoredMessage -> InChan Work -> Gtk.ButtonClickedCallback
onSendMessageClicked convId textView messageViewStore workChan = do
  buf <- get textView #buffer
  mText <- get buf #text
  case mText of
    Nothing -> pure ()
    Just text -> do
      let msg = M.GenericMessage'Text (Proto.defMessage & #content .~ text)
          sendMsg = execute . Opts.SendMessage $ Opts.SendMessageOptions convId msg
      queueActionWithWaitLoopSimple workChan sendMsg $ \eithErr -> do
        runM . logAndThrowPolysemy $ fromEitherStringified "failed to send message" eithErr
        set buf [#text := ""]
        refreshMessageList messageViewStore convId workChan

createEmptyMessageItem :: Gtk.SignalListItemFactorySetupCallback
createEmptyMessageItem msgListItem = do
  box <-
    new Gtk.Box $
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 10,
        #marginTop := 10,
        #marginBottom := 10,
        #marginStart := 10,
        #marginEnd := 10
      ]

  senderStyle <- Pango.attrListNew
  Pango.attrListInsert senderStyle
    =<< Pango.attrWeightNew Pango.WeightBold
  sender <-
    new Gtk.Label $
      [ #name := "sender",
        #halign := Gtk.AlignStart,
        #attributes := senderStyle
      ]

  message <-
    new Gtk.Label $
      [ #name := "message",
        #halign := Gtk.AlignStart
      ]
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

  let senderId = StoredMessage.smSenderUser msg
  liftIO . queueActionWithWaitLoopSimple workChan (getUserName senderId) $
    \e -> runM . logAndThrowPolysemy $ do
      mUsername <- Error.mapError (("failed to get username" <>) . show) . Error.fromEither $ e
      username :: Text <- Error.note ("Sender not found: " <> show senderId) mUsername
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

getUserName :: Members '[Backend, Store, Error WireCLIError] r => Qualified UserId -> Sem r (Maybe Text)
getUserName quid = do
  cred <- Store.getCredsOrErr
  -- TODO: Get this from store and only call backend when the user is not in
  -- store.
  mUser <- Backend.getUser cred quid
  pure $ fromName . profileName <$> mUser

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

populateConvListModel :: Gio.SeqStore Conversation -> Either WireCLIError [Conversation] -> IO ()
populateConvListModel _ (Left err) = print err
populateConvListModel model (Right convs) = do
  putStrLn "populate model"
  Gio.replaceList model convs

createEmptyConvItem :: Gtk.ListItem -> IO ()
createEmptyConvItem convListItem = do
  label <- new Gtk.Label [#halign := Gtk.AlignStart, #hexpand := False]
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

convName :: Members '[Backend, Store, Error WireCLIError] r => Conversation -> Sem r Text
convName conv =
  case cnvName conv of
    Just n -> pure n
    Nothing ->
      case (cmOthers (cnvMembers conv), cnvType conv) of
        (_, SelfConv) ->
          -- Name the self conversation as notes, other clients hide this.
          pure "Notes"
        ([othMem], _) ->
          fromMaybe "Deleted User" <$> getUserName (omQualifiedId othMem)
        _ -> pure "Unnamed Conversation"

convSelected :: InChan Work -> Gtk.Label -> Gio.SeqStore StoredMessage -> Gtk.TextView -> Gtk.Button -> Gio.SeqStore Conversation -> Gtk.SingleSelection -> Word32 -> Word32 -> IO ()
convSelected workChan convNameLabel messageViewStore newMessageTextView sendMessageButton store selection _ _ = do
  selectedPos <- get selection #selected
  putStrLn $ "Conv selected: " <> show selectedPos
  Gio.seqStoreLookup store (fromIntegral selectedPos) >>= \case
    Nothing -> logAndThrow "Selected conv out of bounds"
    Just conv -> loadMessages workChan convNameLabel messageViewStore newMessageTextView sendMessageButton conv

loadMessages :: InChan Work -> Gtk.Label -> Gio.SeqStore StoredMessage -> Gtk.TextView -> Gtk.Button -> Conversation -> IO ()
loadMessages workChan convNameLabel messageViewStore newMessageTextView sendMessageButton conv = do
  newMessageBuffer <- get newMessageTextView #buffer
  set newMessageBuffer [#text := ""]

  queueActionWithWaitLoopSimple workChan (convName conv) $ \e -> do
    name <- runM . logAndThrowPolysemy $ fromEitherStringified "Failed to get conv name" e
    set convNameLabel [#label := name]

  refreshMessageList messageViewStore (cnvQualifiedId conv) workChan

  void $ on sendMessageButton #clicked $ onSendMessageClicked (cnvQualifiedId conv) newMessageTextView messageViewStore workChan

refreshMessageList :: Gio.SeqStore StoredMessage -> Qualified ConvId -> InChan Work -> IO ()
refreshMessageList messageViewStore convId workChan = do
  let getMessages = execute . Opts.ListMessages $ Opts.ListMessagesOptions convId 100
  queueActionWithWaitLoopSimple workChan getMessages $ \e -> do
    messages <- runM . logAndThrowPolysemy $ fromEitherStringified "Failed to get messages: " e
    Gio.replaceList messageViewStore messages

fromEitherStringified :: Member (Error String) r => String -> Either WireCLIError a -> Sem r a
fromEitherStringified prefix = Error.mapError ((prefix <>) . show) . Error.fromEither
