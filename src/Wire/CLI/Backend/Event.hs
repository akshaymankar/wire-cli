{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Wire.CLI.Backend.Event where

import Data.Aeson (parseJSON, (.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime)
import Wire.CLI.Backend.Client
import Wire.CLI.Backend.CommonTypes
import Wire.CLI.Backend.Connection
import Wire.CLI.Backend.Conv
import Wire.CLI.Backend.User
import Wire.CLI.Properties
import Wire.CLI.Util.ByteStringJSON
import Wire.CLI.Util.JSONStrategy

data ExtensibleEvent
  = KnownEvent Event
  | UnknownEvent String Aeson.Value
  deriving (Show, Eq)

instance FromJSON ExtensibleEvent where
  parseJSON v =
    pure $ case Aeson.fromJSON v of
      Aeson.Success e -> KnownEvent e
      Aeson.Error err -> UnknownEvent err v

data Event
  = EventUser UserEvent
  | EventUserProperty UserPropertyEvent
  | EventConv ConvEvent
  | EventTeam TeamEvent
  deriving (Show, Eq, Generic)

instance FromJSON Event where
  parseJSON = Aeson.withObject "Event" $ \o -> do
    typ :: Text <- o .: "type"
    if
        | "user.properties" `Text.isPrefixOf` typ -> EventUserProperty <$> parseJSON (Aeson.Object o)
        | "user" `Text.isPrefixOf` typ -> EventUser <$> parseJSON (Aeson.Object o)
        | "conversation" `Text.isPrefixOf` typ -> EventConv <$> parseJSON (Aeson.Object o)
        | "team" `Text.isPrefixOf` typ -> EventTeam <$> parseJSON (Aeson.Object o)
        | otherwise -> fail $ "Unexpected event type: " <> Text.unpack typ

data UserEvent
  = EventUserUpdate User
  | EventUserIdentityRemove User
  | EventUserConnection ConnectionEvent
  | EventUserPushRemove PushTokenRemoveEvent
  | EventUserDelete UserId
  | EventUserClientAdd Client
  | EventUserClientRemove ClientId
  deriving (Show, Eq, Generic)

instance FromJSON UserEvent where
  parseJSON = Aeson.withObject "UserEvent" $ \o -> do
    typ :: Text <- o .: "type"
    case typ of
      "user.update" -> EventUserUpdate <$> o .: "user"
      "user.identity-remove" -> EventUserIdentityRemove <$> o .: "user"
      "user.connection" -> EventUserConnection <$> parseJSON (Aeson.Object o)
      "user.push-remove" -> EventUserPushRemove <$> parseJSON (Aeson.Object o)
      "user.delete" -> EventUserDelete <$> o .: "id"
      "user.client-add" -> EventUserClientAdd <$> o .: "client"
      "user.client-remove" -> EventUserClientRemove <$> o .: "id"
      _ -> fail $ "Unexpected user event type: " <> Text.unpack typ

data UserPropertyEvent
  = EventUserPropertySet SetPropertyEvent
  | EventUserPropertyDelete DeletePropertyEvent
  deriving (Show, Eq, Generic)

instance FromJSON UserPropertyEvent where
  parseJSON = Aeson.withObject "UserPropertyEvent" $ \o -> do
    typ :: Text <- o .: "type"
    case typ of
      "user.properties-set" -> EventUserPropertySet <$> parseJSON (Aeson.Object o)
      "user.properties-delete" -> EventUserPropertyDelete <$> parseJSON (Aeson.Object o)
      _ -> fail $ "Unexpected user property event type: " <> Text.unpack typ

data ConvEvent = ConvEvent
  { convEventConvId :: ConvId,
    convEventTime :: UTCTime,
    convEventFrom :: UserId,
    convEventData :: ConvEventData
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "convEvent" ConvEvent

data ConvEventData
  = EventConvCreate ConvCreateEvent
  | EventConvDelete
  | EventConvRename Name
  | EventConvMemberJoin MemberJoinEvent
  | EventConvMemberLeave [UserId]
  | EventConvMemberUpdate MemberUpdateEvent
  | EventConvConnectRequest ConnectRequestEvent
  | EventConvTyping TypingStatus
  | EventConvOtrMessageAdd OtrMessage
  | EventConvAccessUpdate AccessEvent
  | EventConvCodeUpdate Text
  | EventConvCodeDelete
  | EventConvRecieptModeUpdate ConvRecieptMode
  | EventConvMessageTimerUpdate MessageTimer
  | EventConvGenericMessage
  | EventConvOtrError OtrError
  deriving (Show, Eq, Generic)

instance FromJSON ConvEventData where
  parseJSON = Aeson.withObject "ConvEventData" $ \o -> do
    typ :: Text <- o .: "type"
    case typ of
      "conversation.create" -> EventConvCreate <$> o .: "data"
      "conversation.delete" -> pure EventConvDelete
      "conversation.rename" -> EventConvRename <$> o .: "name"
      "conversation.member-join" -> EventConvMemberJoin <$> parseJSON (Aeson.Object o)
      "conversation.member-leave" -> EventConvMemberLeave <$> o .: "user_ids"
      "conversation.member-update" -> EventConvMemberUpdate <$> parseJSON (Aeson.Object o)
      "conversation.connect-request" -> EventConvConnectRequest <$> parseJSON (Aeson.Object o)
      "conversation.typing" -> EventConvTyping <$> o .: "status"
      "conversation.otr-message-add" -> EventConvOtrMessageAdd <$> parseJSON (Aeson.Object o)
      "conversation.access-update" -> EventConvAccessUpdate <$> parseJSON (Aeson.Object o)
      "conversation.code-update" -> EventConvCodeUpdate <$> parseJSON (Aeson.Object o)
      "conversation.code-delete" -> pure EventConvCodeDelete
      "conversation.receipt-mode-update" -> EventConvRecieptModeUpdate <$> o .: "receipt_mode"
      "conversation.message-timer-update" -> EventConvMessageTimerUpdate <$> o .: "message_timer"
      "conversation.generic-message" -> pure EventConvGenericMessage
      "conversation.otr-error" -> EventConvOtrError <$> o .: "error"
      _ -> fail $ "Unexpected conversation event type: " <> Text.unpack typ

data TeamEvent = TeamEvent
  { teamEventTeam :: TeamId,
    teamEventData :: TeamEventData
  }
  deriving (Show, Eq, Generic)

instance FromJSON TeamEvent where
  parseJSON = Aeson.withObject "TeamEvent" $ \o ->
    TeamEvent <$> o .: "team" <*> parseJSON (Aeson.Object o)

data TeamEventData
  = EventTeamUpdate TeamData
  | EventTeamMemberJoin UserId
  | EventTeamMemberLeave UserId
  | EventTeamMemberUpdate UserId
  deriving (Show, Eq, Generic)

instance FromJSON TeamEventData where
  parseJSON = Aeson.withObject "TeamEventData" $ \o -> do
    typ :: Text <- o .: "type"
    dat :: Aeson.Object <- o .: "data"
    case typ of
      "team.update" -> EventTeamUpdate <$> parseJSON (Aeson.Object o)
      "team.member-join" -> EventTeamMemberJoin <$> dat .: "user"
      "team.member-leave" -> EventTeamMemberLeave <$> dat .: "user"
      "team.member-update" -> EventTeamMemberUpdate <$> dat .: "user"
      _ -> fail $ "Unexpected team event type: " <> Text.unpack typ

data ConnectionEvent = ConnectionEvent
  { connectionEventConnection :: Connection,
    connectionEventPrev :: Maybe Relation,
    connectionEventName :: Maybe Name
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "connectionEvent" ConnectionEvent

newtype PushToken = PushToken Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PushTokenRemoveEvent = PushTokenRemoveEvent
  { ptreToken :: PushToken,
    ptreSenderId :: Text,
    ptreClient :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "ptre" PushTokenRemoveEvent

data SetPropertyEvent
  = SetReadReciept ReadReciept
  | SetFolders [Folder]
  deriving (Show, Eq, Generic)

data PropertyKey
  = PropertyKeyReadReciept
  | PropertyKeyFolders

instance FromJSON PropertyKey where
  parseJSON = Aeson.withText "PropertyKey" $ \case
    "WIRE_READ_RECIEPT" -> pure PropertyKeyReadReciept
    "labels" -> pure PropertyKeyFolders
    t -> fail $ "Unexpected property key: " <> Text.unpack t

instance FromJSON SetPropertyEvent where
  parseJSON = Aeson.withObject "SetPropertyEvent" $ \o -> do
    propKey <- o .: "key"
    case propKey of
      PropertyKeyReadReciept -> SetReadReciept <$> o .: "value"
      PropertyKeyFolders ->
        SetFolders <$> do
          val <- o .: "value"
          Aeson.withObject "Folders Value" (.: "labels") val

data DeletePropertyEvent
  = DeleteReadReciept
  | DeleteFolders
  deriving (Show, Eq, Generic)

instance FromJSON DeletePropertyEvent where
  parseJSON = Aeson.withObject "DeletePropertyEvent" $ \o -> do
    propKey <- o .: "key"
    pure $ case propKey of
      PropertyKeyReadReciept -> DeleteReadReciept
      PropertyKeyFolders -> DeleteFolders

-- | TODO: Reconcile with 'Wire.CLI.Backend.Conv.Conv'
data ConvCreateEvent = ConvCreateEvent
  { cceId :: ConvId,
    cceName :: Maybe Name,
    cceCreator :: UserId,
    cceConvType :: ConvType,
    cceTeam :: Maybe TeamId,
    cceMuted :: Bool,
    cceMutedTime :: UTCTime,
    cceArchived :: Bool,
    cceArchivedTime :: UTCTime,
    cceAccess :: Set Access,
    cceAccessRole :: Maybe AccessRole,
    cceLink :: Maybe Text,
    cceMessageTimer :: Maybe NominalDiffTime,
    cceMembers :: Map UserId ConvRole,
    cceReceiptMode :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "cce" ConvCreateEvent

data MemberJoinEvent = MemberJoinEvent
  { -- | This field is redundant
    mjeUserIds :: [UserId],
    mjeUsers :: Map UserId ConvRole,
    mjeFirstEvent :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON MemberJoinEvent where
  parseJSON = Aeson.withObject "MemberJoinEvent" $ \o ->
    MemberJoinEvent
      <$> o .: "user_ids"
      <*> o .: "users"
      <*> (Text.isPrefixOf "1." <$> o .: "id")

newtype MutedStatus = MutedStatus Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data MemberUpdateEvent = MemberUpdateEvent
  { mueTarget :: UserId,
    mueHidden :: Maybe Bool,
    mueHiddenRef :: Maybe Text,
    mueOtrMuted :: Maybe Bool,
    mueOtrMutedStatus :: Maybe MutedStatus,
    mueOtrMutedRef :: Maybe Text,
    mueOtrArchived :: Maybe Bool,
    mueOtrArchivedRef :: Maybe Text,
    mueConversationRole :: Maybe ConvRole
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "mue" MemberUpdateEvent

data ConnectRequestEvent = ConnectRequestEvent
  { creMessage :: Text,
    creRecipient :: UserId,
    creName :: Name,
    creEmail :: Maybe Email
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "cre" ConnectRequestEvent

data TypingStatus
  = StartedTyping
  | StoppedTyping
  deriving (Show, Eq, Generic)

instance FromJSON TypingStatus where
  parseJSON = Aeson.withText "TypingStatus" $ \case
    "started" -> pure StartedTyping
    "stopped" -> pure StoppedTyping
    t -> fail $ "Invalid TypingStatus: " <> show t

data OtrMessage = OtrMessage
  { otrSender :: ClientId,
    otrRecipient :: ClientId,
    otrText :: Base64ByteString,
    otrData :: Maybe Base64ByteString
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "otr" OtrMessage

data AccessEvent = AccessEvent
  { accessEventAccess :: Set Access,
    accessEventAccessRole :: AccessRole
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "accessEvent" AccessEvent

newtype ConvRecieptMode = ConvRecieptMode Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype MessageTimer = MessageTimer Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | TODO: Make this from Protobuf definitions at https://github.com/wireapp/generic-message-proto
newtype GenericMessage = GenericMessage ByteString

data OtrError
  = OtrErrorDuplicate
  | OtrErrorDecryptionError DecryptionErrorData
  | OtrErrorIdentityChangedError IdentityChangedData
  deriving (Show, Eq, Generic)

instance FromJSON OtrError where
  parseJSON = Aeson.withObject "OtrError" $ \o -> do
    typ :: Text <- o .: "type"
    case typ of
      "otr-error.decryption-error" -> OtrErrorDecryptionError <$> parseJSON (Aeson.Object o)
      "otr-error.identity-changed-error" -> OtrErrorIdentityChangedError <$> parseJSON (Aeson.Object o)
      "otr-error.duplicate" -> pure OtrErrorDuplicate
      _ -> fail $ "Unexpected OtrError: " <> Text.unpack typ

data IdentityChangedData = IdentityChangedData
  { icdFrom :: UserId,
    icdSender :: ClientId
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "icd" IdentityChangedData

data DecryptionErrorData = DecryptionErrorData
  { dedFrom :: UserId,
    dedSender :: ClientId,
    dedMsg :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "ded" DecryptionErrorData

data TeamData = TeamData
  { teamDataName :: Name,
    teamDataIcon :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "teamData" TeamData
