{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Wire.CLI.Message where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Coerce (coerce)
import Data.Id (ClientId (ClientId), UserId, idToText)
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis))
import Data.Key (FoldableWithKey, Key, Keyed, TraversableWithKey)
import qualified Data.Key as Key
import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Lens.Family2 ((&), (.~))
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Proto.Messages (GenericMessage)
import qualified System.CryptoBox as CBox
import Wire.API.Message (ClientMismatch (cmismatchTime, missingClients), NewOtrMessage (NewOtrMessage), OtrRecipients (OtrRecipients), UserClientMap (..), MessageNotSent (MessageNotSentClientMissing))
import Wire.API.User (selfUser, userId)
import Wire.API.User.Client (UserClientPrekeyMap (getUserClientPrekeyMap))
import Wire.API.User.Client.Prekey (Prekey (prekeyKey))
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import qualified Wire.CLI.Options as Opts
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store
import Wire.CLI.UUIDGen (UUIDGen)
import qualified Wire.CLI.UUIDGen as UUIDGen
import qualified Wire.CLI.User as User

-- TODO: Store the sent message
send :: Members [Store, Backend, CryptoBox, UUIDGen, Error WireCLIError] r => Opts.SendMessageOptions -> Sem r ()
send opts = do
  creds <- Store.getCreds >>= Error.note WireCLIError.NotLoggedIn
  clientId <- Store.getClientId >>= Error.note (WireCLIError.ErrorInvalidState WireCLIError.NoClientFound)
  (sentMessage, time) <- send' creds clientId opts
  self <- User.getSelf (Opts.GetSelfOptions False)
  Store.addMessage (Opts.sendMessageConv opts) (Store.StoredMessage (userId . selfUser $ self) clientId (fromUTCTimeMillis time) $ Store.ValidMessage sentMessage)

-- TODO: Loop a few times before giving up
-- TODO: Save the known users and clients and use them for the first time message
-- TODO: Handle other kinds of mismatches
send' :: Members [Store, Backend, CryptoBox, UUIDGen, Error WireCLIError] r => Backend.ServerCredential -> ClientId -> Opts.SendMessageOptions -> Sem r (GenericMessage, UTCTimeMillis)
send' creds clientId (Opts.SendMessageOptions conv plainMsg) = do
  messageId <- UUIDGen.genV4
  let emptyOtrMsg = NewOtrMessage clientId mempty False False Nothing Nothing Nothing
      messageWithId =
        Proto.defMessage
          & #messageId .~ UUID.toText messageId
          & #maybe'content .~ Just plainMsg
  firstResponse <- Backend.sendOtrMessage creds conv emptyOtrMsg
  case firstResponse of
    Right cm -> pure (messageWithId, cmismatchTime cm)
    Left (MessageNotSentClientMissing cm) -> do
      rcpts <-
        Backend.getPrekeyBundles creds (missingClients cm)
          >>= Key.traverseWithKey getOrCreateSession . keyedUCMapDropNothings . coerce . getUserClientPrekeyMap
          >>= mkRecipients messageWithId . coerce

      let otrMsg = NewOtrMessage clientId rcpts False False Nothing Nothing Nothing
      secondResponse <- Backend.sendOtrMessage creds conv otrMsg

      case secondResponse of
        Right cm2 -> pure (messageWithId, cmismatchTime cm2)
        _ -> error $ "Unexpected response: " <> show secondResponse
    Left _ -> error $ "Unhandled response: " <> show firstResponse

getOrCreateSession :: Members [CryptoBox, Error WireCLIError] r => Key KeyedUCMap -> Prekey -> Sem r CBox.Session
getOrCreateSession key prekey = do
  let sessionId = uncurry mkSessionId key
  sessionRes <- CryptoBox.getSession sessionId
  pkBS <-
    Error.mapError WireCLIError.InvalidPrekey
      . Error.fromEither
      . Base64.decodeBase64
      . Text.encodeUtf8
      $ prekeyKey prekey
  case CryptoBox.resultToEither sessionRes of
    Right ses -> pure ses
    Left CBox.NoSession ->
      CryptoBox.resultToError =<< CryptoBox.sessionFromPrekey sessionId pkBS
    Left cerr ->
      Error.throw $ WireCLIError.UnexpectedCryptoBoxError cerr

mkSessionId :: UserId -> ClientId -> CBox.SID
mkSessionId uid (ClientId cid) =
  CBox.SID $ Text.encodeUtf8 $ idToText uid <> "_" <> cid

mkRecipients :: Members [CryptoBox, Error WireCLIError] r => GenericMessage -> UserClientMap CBox.Session -> Sem r OtrRecipients
mkRecipients plainMsg sessionMap =
  OtrRecipients
    <$> traverse
      ( \session -> do
          CryptoBox.encrypt session (Proto.encodeMessage plainMsg)
            & (>>= CryptoBox.resultToError)
            & fmap Base64.encodeBase64
            -- TODO: Log here if saving session fails
            & (<* CryptoBox.save session)
      )
      sessionMap

decryptMessage :: Members '[CryptoBox] r => CBox.SID -> ByteString -> Sem r (Either (CBox.Result ()) (CBox.Session, ByteString))
decryptMessage sid msg = do
  sessionRes <- CryptoBox.getSession sid
  case CryptoBox.resultToEither sessionRes of
    Right ses -> do
      fmap (ses,) . CryptoBox.resultToEither <$> CryptoBox.decrypt ses msg
    Left CBox.NoSession ->
      CryptoBox.resultToEither <$> CryptoBox.sessionFromMessage sid msg
    Left e ->
      pure $ Left e

-- * User Client Map Traversal Utils

-- | 'UserClientMap' with instances of 'Key', 'Keyed', 'FoldableWithKey' and
-- 'TraversableWithKey'
newtype KeyedUCMap a = KeyedUCMap (UserClientMap a)
  deriving stock (Show, Eq, Foldable, Traversable)
  deriving newtype (Functor)

type instance Key KeyedUCMap = (UserId, ClientId)

instance Keyed KeyedUCMap where
  mapWithKey f = coerce $ Map.mapWithKey (Map.mapWithKey . curry f)

instance FoldableWithKey KeyedUCMap where
  foldrWithKey f b = Map.foldrWithKey (flip . Map.foldrWithKey . curry f) b . coerce

instance TraversableWithKey KeyedUCMap where
  traverseWithKey f =
    fmap coerce
      . Map.traverseWithKey (Map.traverseWithKey . curry f)
      . coerce

keyedUCMapDropNothings :: KeyedUCMap (Maybe a) -> KeyedUCMap a
keyedUCMapDropNothings (KeyedUCMap (UserClientMap m)) =
  coerce . flip Map.map m $ transformFilter id
  where
    transformFilter :: (a -> Maybe b) -> Map.Map k a -> Map.Map k b
    transformFilter f = snd . Map.mapEither (maybe (Left ()) Right . f)
