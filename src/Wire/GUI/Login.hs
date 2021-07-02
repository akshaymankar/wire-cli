{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Wire.GUI.Login where

import Control.Concurrent.Chan.Unagi (InChan)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GI.Gtk (AttrOp ((:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified Network.URI as URI
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Options as Opts
import Wire.GUI.Worker (Work (..))
import qualified Wire.GUI.Worker as Worker

{-# ANN module ("HLint: ignore Redundant $" :: String) #-}

mkLoginBox :: IO () -> InChan Work -> IO Gtk.Box
mkLoginBox loginSuccessCallback workChan = do
  mainBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 10,
        #marginTop := 10,
        #marginBottom := 10,
        #marginStart := 10,
        #marginEnd := 10,
        #valign := Gtk.AlignCenter,
        #baselinePosition := Gtk.BaselinePositionCenter
      ]
  inputsBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #spacing := 10
      ]
  actionsBox <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal,
        #spacing := 10,
        #halign := Gtk.AlignCenter
      ]

  loginLabel <- new Gtk.Label [#label := "Wire"]
  errorLabel <-
    new
      Gtk.Label
      [ #visible := False,
        #xalign := 0,
        #ellipsize := Pango.EllipsizeModeEnd,
        #selectable := True
      ]
  backend <- new Gtk.Entry [#placeholderText := "Backend, e.g.: https://nginz-https.example.com"]
  identity <- new Gtk.Entry [#placeholderText := "Username or email"]
  password <- new Gtk.PasswordEntry [#placeholderText := "Password"]
  -- Enable this for easy testing!
  -- flip set [#text := "http://localhost:8080"] =<< Gtk.toEditable backend

  Gtk.boxAppend inputsBox loginLabel
  Gtk.boxAppend inputsBox errorLabel
  Gtk.boxAppend inputsBox backend
  Gtk.boxAppend inputsBox identity
  Gtk.boxAppend inputsBox password

  loginButton <- new Gtk.Button [#label := "Login"]
  createAccount <- new Gtk.Button [#label := "Create Account"]

  Gtk.boxAppend actionsBox loginButton
  Gtk.boxAppend actionsBox createAccount

  Gtk.boxAppend mainBox inputsBox
  Gtk.boxAppend mainBox actionsBox

  _ <- on loginButton #clicked (onLoginClicked mainBox backend identity password errorLabel loginSuccessCallback workChan)
  pure mainBox

getTextFromEntry :: Gtk.Entry -> IO Text
getTextFromEntry entry = flip get #text =<< Gtk.toEditable entry

onLoginClicked :: Gtk.Box -> Gtk.Entry -> Gtk.Entry -> Gtk.PasswordEntry -> Gtk.Label -> IO () -> InChan Work -> Gtk.ButtonClickedCallback
onLoginClicked mainBox backend identity password errorLabel loginSuccessCallback workChan = do
  set mainBox [#sensitive := False]
  maybeBackendURI <- fmap (URI.parseURI . Text.unpack) $ flip get #text =<< get backend #buffer
  identityText <- getTextFromEntry identity
  let loginIdentity =
        if "@" `Text.isInfixOf` Text.tail identityText
          then Opts.LoginEmail identityText
          else Opts.LoginHandle identityText
  passwordText <- flip get #text =<< Gtk.toEditable password
  case maybeBackendURI of
    Nothing -> do
      backendSc <- Gtk.widgetGetStyleContext backend
      Gtk.styleContextAddClass backendSc "error"
      set mainBox [#sensitive := True]
      set errorLabel $
        [ #label := "Invalid backend URL",
          #visible := True
        ]
    Just uri -> do
      let loginRequest =
            Opts.Login (Opts.LoginOptions uri loginIdentity passwordText)
          loginCallback = onLoginResponse mainBox errorLabel loginSuccessCallback
      Worker.queueCommand workChan loginRequest loginCallback

onLoginResponse :: Gtk.Box -> Gtk.Label -> IO () -> Either WireCLIError (Maybe Text) -> IO ()
onLoginResponse mainBox errorLabel successCallback eitherMaybeFailure = do
  case eitherMaybeFailure of
    Left err -> showError . Text.pack $ show err
    Right (Just failure) -> showError failure
    Right Nothing -> successCallback
  where
    showError :: Text -> IO ()
    showError err = do
      Text.putStrLn err
      set errorLabel $
        [ #label := "Failed to login: " <> err,
          #visible := True
        ]
      set mainBox [#sensitive := True]
