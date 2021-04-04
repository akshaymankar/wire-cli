{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Wire.CLI.Message where

import Data.ByteString (ByteString)
import Data.Key (Key)
import qualified Data.Key as Key
import qualified Data.ProtoLens as Proto
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Lens.Family2 ((&), (.~))
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Proto.Messages (GenericMessage)
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Backend.Client (ClientId (..))
import Wire.CLI.Backend.Message
import Wire.CLI.Backend.Prekey (Prekey)
import Wire.CLI.Backend.User (UserId (..))
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.CLI.UUIDGen (UUIDGen)
import qualified Wire.CLI.UUIDGen as UUIDGen
import Wire.CLI.Util.ByteStringJSON (Base64ByteString (..))

send :: Members [Store, Backend, CryptoBox, UUIDGen, Error WireCLIError] r => Opts.SendMessageOptions -> Sem r ()
send opts = do
  creds <- Store.getCreds >>= Error.note WireCLIError.NotLoggedIn
  clientId <- Store.getClientId >>= Error.note (WireCLIError.ErrorInvalidState WireCLIError.NoClientFound)
  send' creds clientId opts

-- TODO: Loop a few times before giving up
-- TODO: Save the known users and clients and use them for the first time message
-- TODO: Handle other kinds of mismatches
send' :: Members [Store, Backend, CryptoBox, UUIDGen, Error WireCLIError] r => Backend.ServerCredential -> Backend.ClientId -> Opts.SendMessageOptions -> Sem r ()
send' creds clientId (Opts.SendMessageOptions conv plainMsg) = do
  messageId <- UUIDGen.genV4
  let emptyOtrMsg = mkNewOtrMessage clientId (Recipients mempty)
      messageWithId =
        Proto.defMessage
          & #messageId .~ UUID.toText messageId
          & #maybe'content .~ Just plainMsg
  firstResponse <- Backend.sendOtrMessage creds conv emptyOtrMsg
  case firstResponse of
    OtrMessageResponseSuccess -> pure ()
    OtrMessageResponseClientMismatch cm -> do
      rcpts <-
        Backend.getPrekeyBundles creds (cmMissing cm)
          >>= Key.traverseWithKey getOrCreateSession . prekeyBundles
          >>= mkRecipients messageWithId

      let otrMsg = mkNewOtrMessage clientId rcpts
      secondResponse <- Backend.sendOtrMessage creds conv otrMsg

      case secondResponse of
        OtrMessageResponseSuccess -> pure ()
        _ -> error $ "Unexpected response: " <> show secondResponse

getOrCreateSession :: Members [CryptoBox, Error WireCLIError] r => Key UserClientMap -> Prekey -> Sem r CBox.Session
getOrCreateSession key prekey = do
  let sessionId = uncurry mkSessionId key
  sessionRes <- CryptoBox.getSession sessionId
  case CryptoBox.resultToEither sessionRes of
    Right ses -> pure ses
    Left CBox.NoSession ->
      CryptoBox.resultToError =<< CryptoBox.sessionFromPrekey sessionId prekey
    Left cerr ->
      Error.throw $ WireCLIError.UnexpectedCryptoBoxError cerr

mkSessionId :: UserId -> ClientId -> CBox.SID
mkSessionId (UserId uid) (ClientId cid) =
  CBox.SID $ Text.encodeUtf8 $ uid <> "_" <> cid

mkRecipients :: Members [CryptoBox, Error WireCLIError] r => GenericMessage -> UserClientMap CBox.Session -> Sem r Recipients
mkRecipients plainMsg sessionMap =
  Recipients
    <$> traverse
      ( \session -> do
          CryptoBox.encrypt session (Proto.encodeMessage plainMsg)
            & (>>= CryptoBox.resultToError)
            & fmap Base64ByteString
            -- TODO: Log here if saving session fails
            & (<* CryptoBox.save session)
      )
      sessionMap

decryptMessage :: Members '[CryptoBox, Error WireCLIError] r => CBox.SID -> ByteString -> Sem r (CBox.Session, ByteString)
decryptMessage sid msg = do
  sessionRes <- CryptoBox.getSession sid
  case CryptoBox.resultToEither sessionRes of
    Right ses ->
      (ses,) <$> (CryptoBox.resultToError =<< CryptoBox.decrypt ses msg)
    Left CBox.NoSession ->
      CryptoBox.resultToError =<< CryptoBox.sessionFromMessage sid msg
    Left e ->
      Error.throw $ WireCLIError.UnexpectedCryptoBoxError e
