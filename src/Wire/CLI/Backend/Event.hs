{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Wire.CLI.Backend.Event where

import Data.Aeson (parseJSON)
import qualified Data.Aeson as Aeson
import Data.Id
import Data.Profunctor (dimap)
import Data.ProtoLens.Prism (Prism', prism')
import Data.Schema
import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Family2.Stock (_1)
import Wire.API.Connection (Relation, UserConnection)
import qualified Wire.API.Event.Conversation as Conversation
import qualified Wire.API.Event.Team as Team
import Wire.API.User (Name)
import Wire.API.User.Client (Client)
import Wire.CLI.Backend.Orphans ()
import Wire.CLI.Backend.User
import Wire.CLI.Properties
import Wire.CLI.Util.JSONStrategy
import Wire.CLI.Util.Schema
import Data.Coerce (coerce)

data ExtensibleEvent
  = KnownEvent Event
  | UnknownEvent String Aeson.Value
  deriving (Show, Eq)
  deriving (ToSchema) via NoDoc ExtensibleEvent

instance FromJSON ExtensibleEvent where
  parseJSON v =
    pure $ case Aeson.fromJSON v of
      Aeson.Success e -> KnownEvent e
      Aeson.Error err -> UnknownEvent err v

instance ToJSON ExtensibleEvent where
  toJSON = \case
    KnownEvent ev -> Aeson.toJSON ev
    UnknownEvent _ va -> va

data Event
  = EventUser UserEvent
  | EventUserProperty UserPropertyEvent
  | EventConv Conversation.Event
  | EventTeam Team.Event
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema Event

_EventUser :: Prism' Event UserEvent
_EventUser = prism' EventUser $ \case
  EventUser x -> Just x
  _ -> Nothing

_EventUserProperty :: Prism' Event UserPropertyEvent
_EventUserProperty = prism' EventUserProperty $ \case
  EventUserProperty x -> Just x
  _ -> Nothing

_EventConv :: Prism' Event Conversation.Event
_EventConv = prism' EventConv $ \case
  EventConv x -> Just x
  _ -> Nothing

_EventTeam :: Prism' Event Team.Event
_EventTeam = prism' EventTeam $ \case
  EventTeam x -> Just x
  _ -> Nothing

instance ToSchema Event where
  schema =
    object "Event" $
      dimap (\o -> (getEventType o, o)) snd $
        bind
          (fst .= field "type" schema)
          ( snd
              .= dispatch
                ( \case
                    EventTypeUser _ -> tag _EventUser userEventObjectSchema
                    EventTypeUserProperty _ -> tag _EventUserProperty userPropertyEventObjectSchema
                    EventTypeConv _ -> tag _EventConv (toObjectSchema schema)
                    EventTypeTeam _ -> tag _EventTeam (coerce (toObjectSchema (schema @(NoDoc Team.Event))))
                )
          )

getEventType :: Event -> EventType
getEventType = \case
  EventUser ue -> EventTypeUser $ getUEType ue
  EventUserProperty upe -> EventTypeUserProperty $ getUPEType upe
  EventConv ev -> EventTypeConv $ Conversation.evtType ev
  EventTeam ev -> EventTypeTeam $ Team.eventType ev

data EventType
  = EventTypeUser UEType
  | EventTypeUserProperty UPEType
  | EventTypeConv Conversation.EventType
  | EventTypeTeam Team.EventType
  deriving (Eq)
  deriving (ToSchema) via NoDoc EventType

instance Enum EventType where
  toEnum i
    | i <= maxUEType = EventTypeUser $ toEnum i
    | i <= maxUEType + maxUPEType = EventTypeUserProperty . toEnum $ i - maxUEType
    | i <= maxUEType + maxUPEType + maxConvEventType = EventTypeConv . toEnum $ i - (maxUEType + maxUPEType)
    | otherwise = EventTypeTeam . toEnum $ i - (maxUEType + maxUPEType + maxConvEventType)
  fromEnum = \case
    EventTypeUser t -> fromEnum t
    EventTypeUserProperty t -> maxUEType + fromEnum t
    EventTypeConv t -> maxUEType + maxUPEType + fromEnum t
    EventTypeTeam t -> maxUEType + maxUPEType + maxConvEventType + fromEnum t

instance Bounded EventType where
  minBound = EventTypeUser $ minBound @UEType
  maxBound = EventTypeTeam $ maxBound @Team.EventType

maxUEType :: Int
maxUEType = fromEnum (maxBound :: UEType)

maxUPEType :: Int
maxUPEType = fromEnum (maxBound :: UPEType)

maxConvEventType :: Int
maxConvEventType = fromEnum (maxBound :: Conversation.EventType)

instance ToJSON EventType where
  toJSON = \case
    EventTypeUser ut -> Aeson.toJSON ut
    EventTypeUserProperty ut -> Aeson.toJSON ut
    EventTypeConv et -> Aeson.toJSON et
    EventTypeTeam et -> Aeson.toJSON et

instance FromJSON EventType where
  parseJSON = Aeson.withText "EventType" $ \typ ->
    if
        | "user.properties" `Text.isPrefixOf` typ -> EventTypeUserProperty <$> parseJSON (Aeson.String typ)
        | "user" `Text.isPrefixOf` typ -> EventTypeUser <$> parseJSON (Aeson.String typ)
        | "conversation" `Text.isPrefixOf` typ -> EventTypeConv <$> parseJSON (Aeson.String typ)
        | "team" `Text.isPrefixOf` typ -> EventTypeTeam <$> parseJSON (Aeson.String typ)
        | otherwise -> fail $ "Unexpected event type: " <> Text.unpack typ

data UserEvent
  = EventUserUpdate UserUpdate
  | EventUserIdentityRemove UserIdentityRemove
  | EventUserConnection ConnectionEvent
  | EventUserPushRemove PushTokenRemoveEvent
  | EventUserDelete UserId
  | EventUserClientAdd Client
  | EventUserClientRemove ClientId
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema UserEvent

instance ToSchema UserEvent where
  schema = object "UserEvent" userEventObjectSchema

userEventObjectSchema :: ObjectSchema SwaggerDoc UserEvent
userEventObjectSchema =
  dimap (\o -> (getUEType o, o)) snd $
    bind
      (fst .= field "type" schema)
      ( snd
          .= dispatch
            ( \case
                UEUpdate -> tag _EventUserUpdate (field "user" schema)
                UEIdentityRemove -> tag _EventUserIdentityRemove (field "user" schema)
                UEConnection -> tag _EventUserConnection connectionEventObjectSchema
                UEPushRemove -> tag _EventUserPushRemove pushTokenRemoveEventObjectSchema
                UEDelete -> tag _EventUserDelete (field "id" schema)
                UEClientAdd -> tag _EventUserClientAdd (field "client" schema)
                UEClientRemove -> tag _EventUserClientRemove (field "id" schema)
            )
      )

_EventUserUpdate :: Prism' UserEvent UserUpdate
_EventUserUpdate = prism' EventUserUpdate $ \case
  EventUserUpdate x -> Just x
  _ -> Nothing

_EventUserIdentityRemove :: Prism' UserEvent UserIdentityRemove
_EventUserIdentityRemove = prism' EventUserIdentityRemove $ \case
  EventUserIdentityRemove x -> Just x
  _ -> Nothing

_EventUserConnection :: Prism' UserEvent ConnectionEvent
_EventUserConnection = prism' EventUserConnection $ \case
  EventUserConnection x -> Just x
  _ -> Nothing

_EventUserPushRemove :: Prism' UserEvent PushTokenRemoveEvent
_EventUserPushRemove = prism' EventUserPushRemove $ \case
  EventUserPushRemove x -> Just x
  _ -> Nothing

_EventUserDelete :: Prism' UserEvent UserId
_EventUserDelete = prism' EventUserDelete $ \case
  EventUserDelete x -> Just x
  _ -> Nothing

_EventUserClientAdd :: Prism' UserEvent Client
_EventUserClientAdd = prism' EventUserClientAdd $ \case
  EventUserClientAdd x -> Just x
  _ -> Nothing

_EventUserClientRemove :: Prism' UserEvent ClientId
_EventUserClientRemove = prism' EventUserClientRemove $ \case
  EventUserClientRemove x -> Just x
  _ -> Nothing

getUEType :: UserEvent -> UEType
getUEType = \case
  EventUserUpdate _ -> UEUpdate
  EventUserIdentityRemove _ -> UEIdentityRemove
  EventUserConnection _ -> UEConnection
  EventUserPushRemove _ -> UEPushRemove
  EventUserDelete _ -> UEDelete
  EventUserClientAdd _ -> UEClientAdd
  EventUserClientRemove _ -> UEClientRemove

data UEType
  = UEUpdate
  | UEIdentityRemove
  | UEConnection
  | UEPushRemove
  | UEDelete
  | UEClientAdd
  | UEClientRemove
  deriving (Eq, Bounded, Enum)
  deriving (ToJSON, FromJSON) via Schema UEType

instance ToSchema UEType where
  schema =
    enum @Text "UEType" $
      mconcat
        [ element "user.update" UEUpdate,
          element "user.identity-remove" UEIdentityRemove,
          element "user.connection" UEConnection,
          element "user.push-remove" UEPushRemove,
          element "user.delete" UEDelete,
          element "user.client-add" UEClientAdd,
          element "user.client-remove" UEClientRemove
        ]

data UserPropertyEvent
  = EventUserPropertySet SetPropertyEvent
  | EventUserPropertyDelete DeletePropertyEvent
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema UserPropertyEvent

instance ToSchema UserPropertyEvent where
  schema = object "UserPropertyEvent" userPropertyEventObjectSchema

userPropertyEventObjectSchema :: ObjectSchema SwaggerDoc UserPropertyEvent
userPropertyEventObjectSchema =
  dimap (\o -> (getUPEType o, o)) snd $
    bind
      (fst .= field "type" (schema @UPEType))
      ( snd
          .= dispatch
            ( \case
                UPESet -> tag _EventUserPropertySet setPropertyEventObjectSchema
                UPEDelete -> tag _EventUserPropertyDelete (field "key" schema)
            )
      )

_EventUserPropertySet :: Prism' UserPropertyEvent SetPropertyEvent
_EventUserPropertySet = prism' EventUserPropertySet $ \case
  EventUserPropertySet spe -> Just spe
  EventUserPropertyDelete _ -> Nothing

_EventUserPropertyDelete :: Prism' UserPropertyEvent DeletePropertyEvent
_EventUserPropertyDelete = prism' EventUserPropertyDelete $ \case
  EventUserPropertySet _ -> Nothing
  EventUserPropertyDelete dpe -> Just dpe

getUPEType :: UserPropertyEvent -> UPEType
getUPEType = \case
  EventUserPropertySet _ -> UPESet
  EventUserPropertyDelete _ -> UPEDelete

data UPEType = UPESet | UPEDelete
  deriving (Bounded, Enum, Eq)
  deriving (ToJSON, FromJSON) via Schema UPEType

instance ToSchema UPEType where
  schema =
    enum @Text "UPEType" $
      mconcat
        [ element "user.properties-set" UPESet,
          element "user.properties-delete" UPEDelete
        ]

data ConnectionEvent = ConnectionEvent
  { connectionEventConnection :: UserConnection,
    connectionEventPrev :: Maybe Relation,
    connectionEventName :: Maybe Name
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema ConnectionEvent

instance ToSchema ConnectionEvent where
  schema = object "ConnectionEvent" connectionEventObjectSchema

connectionEventObjectSchema :: ObjectSchema SwaggerDoc ConnectionEvent
connectionEventObjectSchema =
  ConnectionEvent
    <$> connectionEventConnection .= field "connection" schema
    <*> connectionEventPrev .= maybe_ (optField "prev" schema)
    <*> connectionEventName .= maybe_ (optField "name" schema)

newtype PushToken = PushToken Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data PushTokenRemoveEvent = PushTokenRemoveEvent
  { ptreToken :: PushToken,
    ptreSenderId :: Text,
    ptreClient :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema PushTokenRemoveEvent

instance ToSchema PushTokenRemoveEvent where
  schema = object "PushTokenRemoveEvent" pushTokenRemoveEventObjectSchema

pushTokenRemoveEventObjectSchema :: ObjectSchema SwaggerDoc PushTokenRemoveEvent
pushTokenRemoveEventObjectSchema =
  PushTokenRemoveEvent
    <$> ptreToken .= field "token" schema
    <*> ptreSenderId .= field "sender_id" schema
    <*> ptreClient .= maybe_ (optField "client" schema)

data SetPropertyEvent
  = SetReadReciept ReadReciept
  | SetFolders [Folder]
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema SetPropertyEvent

_SetReadReciept :: Prism' SetPropertyEvent ReadReciept
_SetReadReciept = prism' SetReadReciept $ \case
  SetReadReciept rr -> Just rr
  SetFolders _ -> Nothing

_SetFolders :: Prism' SetPropertyEvent [Folder]
_SetFolders = prism' SetFolders $ \case
  SetFolders fs -> Just fs
  SetReadReciept _ -> Nothing

getPropertyKey :: SetPropertyEvent -> PropertyKey
getPropertyKey = \case
  SetReadReciept _ -> PropertyKeyReadReciept
  SetFolders _ -> PropertyKeyFolders

instance ToSchema SetPropertyEvent where
  schema = object "SetPropertyEvent" setPropertyEventObjectSchema

setPropertyEventObjectSchema :: ObjectSchema SwaggerDoc SetPropertyEvent
setPropertyEventObjectSchema =
  dimap (\o -> (getPropertyKey o, o)) snd $
    bind
      (fst .= field "key" schema)
      ( snd
          .= ( fieldOver _1 "value" . dispatch $ \t ->
                 unnamed $ case t of
                   PropertyKeyReadReciept -> tag _SetReadReciept schema
                   PropertyKeyFolders -> tag _SetFolders (object "Folders" $ field "labels" (array schema))
             )
      )

data PropertyKey
  = PropertyKeyReadReciept
  | PropertyKeyFolders
  deriving (Show, Eq, Bounded, Enum)
  deriving (ToJSON, FromJSON) via Schema PropertyKey

instance ToSchema PropertyKey where
  schema =
    enum @Text "PropertyKey" $
      mconcat
        [ element "WIRE_READ_RECIEPT" PropertyKeyReadReciept,
          element "labels" PropertyKeyFolders
        ]

data DeletePropertyEvent
  = DeleteReadReciept
  | DeleteFolders
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema DeletePropertyEvent

instance ToSchema DeletePropertyEvent where
  schema =
    dimap
      ( \case
          DeleteReadReciept -> PropertyKeyReadReciept
          DeleteFolders -> PropertyKeyFolders
      )
      ( \case
          PropertyKeyReadReciept -> DeleteReadReciept
          PropertyKeyFolders -> DeleteFolders
      )
      (schema @PropertyKey)
