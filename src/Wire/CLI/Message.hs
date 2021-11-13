{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Wire.CLI.Message where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Coerce (coerce)
import Data.Domain
import Data.Id (ClientId (ClientId), UserId, idToText)
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis))
import Data.Key (FoldableWithKey, Key, Keyed, TraversableWithKey)
import qualified Data.Key as Key
import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import Data.Qualified
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Lens.Family2 ((&), (.~))
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Proto.Messages (GenericMessage)
import qualified System.CryptoBox as CBox
import Wire.API.Message
import Wire.API.User
import Wire.API.User.Client
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
  cid <- Store.getClientId >>= Error.note (WireCLIError.ErrorInvalidState WireCLIError.NoClientFound)
  (sentMessage, time) <- send' creds cid opts
  self <- User.getSelf (Opts.GetSelfOptions False)
  let storedMessage =
        Store.StoredMessage
          { Store.smSenderUser = userQualifiedId . selfUser $ self,
            Store.smSenderClient = cid,
            Store.smSentAt = fromUTCTimeMillis time,
            Store.smMessage = Store.ValidMessage sentMessage
          }
  Store.addMessage (Opts.sendMessageConv opts) storedMessage

-- TODO: Loop a few times before giving up
-- TODO: Save the known users and clients and use them for the first time message
-- TODO: Handle other kinds of mismatches
send' :: Members [Store, Backend, CryptoBox, UUIDGen, Error WireCLIError] r => Backend.ServerCredential -> ClientId -> Opts.SendMessageOptions -> Sem r (GenericMessage, UTCTimeMillis)
send' creds cid (Opts.SendMessageOptions conv plainMsg) = do
  messageId <- UUIDGen.genV4
  let emptyOtrMsg =
        QualifiedNewOtrMessage
          { qualifiedNewOtrSender = cid,
            qualifiedNewOtrRecipients = mempty,
            qualifiedNewOtrNativePush = False,
            qualifiedNewOtrTransient = False,
            qualifiedNewOtrNativePriority = Nothing,
            qualifiedNewOtrData = mempty,
            qualifiedNewOtrClientMismatchStrategy = MismatchReportAll
          }
      messageWithId =
        Proto.defMessage
          & #messageId .~ UUID.toText messageId
          & #maybe'content .~ Just plainMsg
  firstResponse <- Backend.sendOtrMessage creds conv emptyOtrMsg
  case firstResponse of
    Right mss -> pure (messageWithId, mssTime mss)
    Left (MessageNotSentClientMissing mss) -> do
      rcpts <-
        Backend.getPrekeyBundles creds (mssMissingClients mss)
          >>= Key.traverseWithKey getOrCreateSession . keyedUCMapDropNothings . coerce . getQualifiedUserClientPrekeyMap
          >>= mkRecipients messageWithId . coerce

      let otrMsg = emptyOtrMsg {qualifiedNewOtrRecipients = rcpts}
      secondResponse <- Backend.sendOtrMessage creds conv otrMsg

      case secondResponse of
        Right mss2 -> pure (messageWithId, mssTime mss2)
        _ -> error $ "Unexpected response: " <> show secondResponse
    Left _ -> error $ "Unhandled response: " <> show firstResponse

getOrCreateSession :: Members [CryptoBox, Error WireCLIError] r => Key KeyedQUCMap -> Prekey -> Sem r CBox.Session
getOrCreateSession (d, u, c) prekey = do
  let sessionId = mkSessionId (Qualified u d) c
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

mkSessionId :: Qualified UserId -> ClientId -> CBox.SID
mkSessionId (Qualified uid domain) (ClientId cid) =
  CBox.SID . Text.encodeUtf8 $ Text.intercalate "_" [domainText domain, idToText uid, cid]

mkRecipients :: Members [CryptoBox, Error WireCLIError] r => GenericMessage -> QualifiedUserClientMap CBox.Session -> Sem r QualifiedOtrRecipients
mkRecipients plainMsg sessionMap =
  coerce
    <$> traverse
      ( \session -> do
          CryptoBox.encrypt session (Proto.encodeMessage plainMsg)
            & (>>= CryptoBox.resultToError)
            -- TODO: Log here if saving session fails
            & (<* CryptoBox.save session)
      )
      (KeyedQUCMap sessionMap)

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

-- | 'QualifiedUserClientMap' with instances of 'Key', 'Keyed', 'FoldableWithKey' and
-- 'TraversableWithKey'
newtype KeyedQUCMap a = KeyedQUCMap (QualifiedUserClientMap a)
  deriving stock (Show, Eq)
  deriving newtype (Functor)

instance Foldable KeyedQUCMap where
  foldMap f (KeyedQUCMap (QualifiedUserClientMap domainMap)) =
    foldMap (foldMap (foldMap f)) domainMap

instance Traversable KeyedQUCMap where
  traverse f (KeyedQUCMap (QualifiedUserClientMap domainMap)) =
    coerce <$> traverse (traverse (traverse f)) domainMap

type instance Key KeyedQUCMap = (Domain, UserId, ClientId)

instance Keyed KeyedQUCMap where
  mapWithKey f =
    coerce $
      Map.mapWithKey (\d -> Map.mapWithKey (\u -> Map.mapWithKey (\c -> f (d, u, c))))

instance FoldableWithKey KeyedQUCMap where
  foldMapWithKey f =
    Map.foldMapWithKey (\d -> Map.foldMapWithKey (\u -> Map.foldMapWithKey (\c -> f (d, u, c)))) . coerce

instance TraversableWithKey KeyedQUCMap where
  traverseWithKey f =
    fmap coerce
      . Map.traverseWithKey (\d -> Map.traverseWithKey (\u -> Map.traverseWithKey (\c -> f (d, u, c))))
      . coerce

keyedUCMapDropNothings :: KeyedQUCMap (Maybe a) -> KeyedQUCMap a
keyedUCMapDropNothings (KeyedQUCMap (QualifiedUserClientMap m)) =
  coerce . flip Map.map m $ Map.map (transformFilter id)
  where
    transformFilter :: (a -> Maybe b) -> Map.Map k a -> Map.Map k b
    transformFilter f = snd . Map.mapEither (maybe (Left ()) Right . f)
