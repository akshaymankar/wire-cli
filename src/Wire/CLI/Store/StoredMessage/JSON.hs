module Wire.CLI.Store.StoredMessage.JSON where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Profunctor (dimap)
import qualified Data.ProtoLens as Proto
import Data.Schema hiding ((.=))
import qualified Data.Schema as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Lens.Family2 (Lens', view, (.~))
import Lens.Family2.Stock (_1)
import Proto.Messages hiding (Text)
import qualified Proto.Messages as M
import Proto.Messages_Fields hiding (id, tag)
import qualified Proto.Messages_Fields as M

genericMessageSchema :: ValueSchema NamedSwaggerDoc GenericMessage
genericMessageSchema =
  object "GenericMessage" $
    pure Proto.defMessage
      <~> messageId .= field "id" schema
      <~> maybe'content .= maybe_ (optField "content" genericMessageContentSchema)

genericMessageContentSchema :: ValueSchema NamedSwaggerDoc GenericMessage'Content
genericMessageContentSchema =
  oneof "GenericMessage'Content" getGMCType $ \case
    GMCTypeText -> tag _GenericMessage'Text textObjectSchema
    GMCTypeImage -> tag _GenericMessage'Image imageAssetObjectSchema
    GMCTypeKnock -> tag _GenericMessage'Knock knockObjectSchema
    GMCTypeLastRead -> tag _GenericMessage'LastRead lastReadObjectSchema
    GMCTypeCleared -> tag _GenericMessage'Cleared clearedObjectSchema
    GMCTypeExternal -> tag _GenericMessage'External externalObjectSchema
    GMCTypeClientAction -> tag _GenericMessage'ClientAction (Prelude.id S..= field "client_action" clientActionObjectSchema)
    GMCTypeCalling -> tag _GenericMessage'Calling callingObjectSchema
    GMCTypeAsset -> tag _GenericMessage'Asset assetObjectSchema
    GMCTypeHidden -> tag _GenericMessage'Hidden messageHideObjectSchema
    GMCTypeLocation -> tag _GenericMessage'Location locationObjectSchema
    GMCTypeDeleted -> tag _GenericMessage'Deleted messageDeleteObjectSchema
    GMCTypeEdited -> tag _GenericMessage'Edited messageEditObjectSchema
    GMCTypeConfirmation -> tag _GenericMessage'Confirmation confirmationObjectSchema
    GMCTypeReaction -> tag _GenericMessage'Reaction reactionObjectSchema
    GMCTypeEphemeral -> tag _GenericMessage'Ephemeral ephemeralObjectSchema
    GMCTypeAvailability -> tag _GenericMessage'Availability availabilityObjectSchema
    GMCTypeComposite -> tag _GenericMessage'Composite compositeObjectSchema
    GMCTypeButtonAction -> tag _GenericMessage'ButtonAction buttonActionObjectSchema
    GMCTypeButtonActionConfirmation -> tag _GenericMessage'ButtonActionConfirmation buttonActionConfirmationObjectSchema

textObjectSchema :: ObjectSchema SwaggerDoc M.Text
textObjectSchema =
  pure Proto.defMessage
    <~> content .= field "content" schema
    <~> linkPreview .= field "link_preview" (array linkPreviewSchema)
    <~> mentions .= field "mentions" (array mentionSchema)
    <~> maybe'quote .= maybe_ (optField "quote" quoteSchema)
    <~> maybe'expectsReadConfirmation .= maybe_ (optField "expectsReadConfirmation" schema)
    <~> maybe'legalHoldStatus .= maybe_ (optField "legalHoldStatus" legalHoldStatusSchema)

imageAssetObjectSchema :: ObjectSchema SwaggerDoc ImageAsset
imageAssetObjectSchema =
  pure Proto.defMessage
    <~> M.tag .= field "tag" schema
    <~> width .= field "width" schema
    <~> height .= field "height" schema
    <~> originalWidth .= field "originalWidth" schema
    <~> originalHeight .= field "originalHeight" schema
    <~> mimeType .= field "mimeType" schema
    <~> size .= field "size" schema
    <~> maybe'otrKey .= maybe_ (optField "otrKey" bytestringSchema)
    <~> maybe'macKey .= maybe_ (optField "macKey" bytestringSchema)
    <~> maybe'mac .= maybe_ (optField "mac" bytestringSchema)
    <~> maybe'sha256 .= maybe_ (optField "sha256" bytestringSchema)

knockObjectSchema :: ObjectSchema SwaggerDoc Knock
knockObjectSchema =
  pure Proto.defMessage
    <~> hotKnock .= field "hotKnock" schema
    <~> maybe'expectsReadConfirmation .= maybe_ (optField "expectsReadConfirmation" schema)
    <~> maybe'legalHoldStatus .= maybe_ (optField "legalHoldStatus" legalHoldStatusSchema)

lastReadObjectSchema :: ObjectSchema SwaggerDoc LastRead
lastReadObjectSchema =
  pure Proto.defMessage
    <~> conversationId .= field "conversationId" schema
    <~> lastReadTimestamp .= field "lastReadTimestamp" schema

clearedObjectSchema :: ObjectSchema SwaggerDoc Cleared
clearedObjectSchema =
  pure Proto.defMessage
    <~> conversationId .= field "conversationId" schema
    <~> clearedTimestamp .= field "clearedTimestamp" schema

externalObjectSchema :: ObjectSchema SwaggerDoc External
externalObjectSchema =
  pure Proto.defMessage
    <~> otrKey .= field "otrKey" bytestringSchema
    <~> maybe'sha256 .= maybe_ (optField "sha256" bytestringSchema)
    <~> maybe'encryption .= maybe_ (optField "encryption" encryptionAlgorithmSchema)

clientActionObjectSchema :: ValueSchema NamedSwaggerDoc ClientAction
clientActionObjectSchema =
  enum @Text "ClientAction" $ mconcat [element "RESET_SESSION" RESET_SESSION]

callingObjectSchema :: ObjectSchema SwaggerDoc Calling
callingObjectSchema =
  pure Proto.defMessage
    <~> content .= field "content" schema

assetSchema :: ValueSchema NamedSwaggerDoc Asset
assetSchema =
  object "Asset" assetObjectSchema

assetObjectSchema :: ObjectSchema SwaggerDoc Asset
assetObjectSchema =
  pure Proto.defMessage
    <~> maybe'original .= maybe_ (optField "original" assetOriginalSchema)
    <~> maybe'status .= maybe_ (optField "status" assetStatusSchema)
    <~> maybe'preview .= maybe_ (optField "preview" assetPreviewSchema)
    <~> maybe'expectsReadConfirmation .= maybe_ (optField "expectsReadConfirmation" schema)
    <~> maybe'legalHoldStatus .= maybe_ (optField "legalHoldStatus" legalHoldStatusSchema)

messageHideObjectSchema :: ObjectSchema SwaggerDoc MessageHide
messageHideObjectSchema =
  pure Proto.defMessage
    <~> conversationId .= field "conversationId" schema
    <~> messageId .= field "messageId" schema

locationObjectSchema :: ObjectSchema SwaggerDoc Location
locationObjectSchema =
  pure Proto.defMessage
    <~> longitude .= field "longitude" floatSchema
    <~> latitude .= field "latitude" floatSchema
    <~> maybe'name .= maybe_ (optField "name" schema)
    <~> maybe'zoom .= maybe_ (optField "zoom" schema)
    <~> maybe'expectsReadConfirmation .= maybe_ (optField "expectsReadConfirmation" schema)
    <~> maybe'legalHoldStatus .= maybe_ (optField "legalHoldStatus" legalHoldStatusSchema)

floatSchema :: ValueSchema NamedSwaggerDoc Float
floatSchema = genericToSchema

messageDeleteObjectSchema :: ObjectSchema SwaggerDoc MessageDelete
messageDeleteObjectSchema =
  pure Proto.defMessage
    <~> messageId .= field "messageId" schema

messageEditObjectSchema :: ObjectSchema SwaggerDoc MessageEdit
messageEditObjectSchema =
  pure Proto.defMessage
    <~> replacingMessageId .= field "replacingMessageId" schema

confirmationObjectSchema :: ObjectSchema SwaggerDoc Confirmation
confirmationObjectSchema =
  pure Proto.defMessage
    <~> type' .= field "type" confirmationTypeSchema
    <~> firstMessageId .= field "firstMessageId" schema
    <~> moreMessageIds .= field "moreMessageIds" (array schema)

confirmationTypeSchema :: ValueSchema NamedSwaggerDoc Confirmation'Type
confirmationTypeSchema =
  enum @Text "Confirmation'Type" $
    mconcat
      [ element "DELIVERED" Confirmation'DELIVERED,
        element "READ" Confirmation'READ
      ]

reactionObjectSchema :: ObjectSchema SwaggerDoc Reaction
reactionObjectSchema =
  pure Proto.defMessage
    <~> maybe'emoji .= maybe_ (optField "emoji" schema)
    <~> messageId .= field "messageId" schema
    <~> maybe'legalHoldStatus .= maybe_ (optField "legalHoldStatus" legalHoldStatusSchema)

ephemeralObjectSchema :: ObjectSchema SwaggerDoc Ephemeral
ephemeralObjectSchema =
  pure Proto.defMessage
    <~> expireAfterMillis .= field "expireAfterMillis" schema
    <~> maybe'content .= maybe_ (optField "content" ephmeralContentSchema)

ephmeralContentSchema :: ValueSchema NamedSwaggerDoc Ephemeral'Content
ephmeralContentSchema =
  oneof "Ephemeral'Content" getECType $ \case
    ECTypeText -> tag _Ephemeral'Text textObjectSchema
    ECTypeImage -> tag _Ephemeral'Image imageAssetObjectSchema
    ECTypeKnock -> tag _Ephemeral'Knock knockObjectSchema
    ECTypeAsset -> tag _Ephemeral'Asset assetObjectSchema
    ECTypeLocation -> tag _Ephemeral'Location locationObjectSchema

availabilityObjectSchema :: ObjectSchema SwaggerDoc Availability
availabilityObjectSchema =
  pure Proto.defMessage
    <~> type' .= field "type" availabilityTypeSchema

availabilityTypeSchema :: ValueSchema NamedSwaggerDoc Availability'Type
availabilityTypeSchema =
  enum @Text "Availability'Type" $
    mconcat
      [ element "NONE" Availability'NONE,
        element "AVAILABLE" Availability'AVAILABLE,
        element "AWAY" Availability'AWAY,
        element "BUSY" Availability'BUSY
      ]

compositeObjectSchema :: ObjectSchema SwaggerDoc Composite
compositeObjectSchema =
  pure Proto.defMessage
    <~> items .= field "items" (array compositeItemSchema)
    <~> maybe'expectsReadConfirmation .= maybe_ (optField "expectsReadConfirmation" schema)
    <~> maybe'legalHoldStatus .= maybe_ (optField "legalHoldStatus" legalHoldStatusSchema)

compositeItemSchema :: ValueSchema NamedSwaggerDoc Composite'Item
compositeItemSchema =
  object "Composite'Item" $
    pure Proto.defMessage
      <~> maybe'content .= maybe_ (optField "content" compositeItemContentSchema)

compositeItemContentSchema :: ValueSchema NamedSwaggerDoc Composite'Item'Content
compositeItemContentSchema =
  oneof "Composite'Item'Content" getCICType $ \case
    CICTypeText -> tag _Composite'Item'Text textObjectSchema
    CICTypeButton -> tag _Composite'Item'Button buttonObjectSchema

buttonObjectSchema :: ObjectSchema SwaggerDoc Button
buttonObjectSchema =
  pure Proto.defMessage
    <~> M.text .= field "text" schema
    <~> M.id .= field "id" schema

buttonActionObjectSchema :: ObjectSchema SwaggerDoc ButtonAction
buttonActionObjectSchema =
  pure Proto.defMessage
    <~> buttonId .= field "buttonId" schema
    <~> referenceMessageId .= field "referenceMessageId" schema

buttonActionConfirmationObjectSchema :: ObjectSchema SwaggerDoc ButtonActionConfirmation
buttonActionConfirmationObjectSchema =
  pure Proto.defMessage
    <~> referenceMessageId .= field "referenceMessageId" schema
    <~> maybe'buttonId .= maybe_ (optField "buttonId" schema)

linkPreviewSchema :: ValueSchema NamedSwaggerDoc LinkPreview
linkPreviewSchema =
  object "LinkPreview" $
    pure Proto.defMessage
      <~> url .= field "url" schema
      <~> urlOffset .= field "urlOffset" schema
      <~> maybe'preview .= maybe_ (optField "preview" linkPreviewPreviewSchema)
      <~> maybe'permanentUrl .= maybe_ (optField "permanentUrl" schema)
      <~> maybe'title .= maybe_ (optField "title" schema)
      <~> maybe'summary .= maybe_ (optField "summary" schema)
      <~> maybe'image .= maybe_ (optField "image" assetSchema)
      <~> maybe'metaData .= maybe_ (optField "metaData" linkPreviewMetaDataSchema)

linkPreviewPreviewSchema :: ValueSchema NamedSwaggerDoc LinkPreview'Preview
linkPreviewPreviewSchema =
  oneof "LinkPreview'Preview" getLPPType $ \LPPTypeArticle ->
    tag _LinkPreview'Article articleObjectSchema

articleObjectSchema :: ObjectSchema SwaggerDoc Article
articleObjectSchema =
  pure Proto.defMessage
    <~> permanentUrl .= field "permanentUrl" schema
    <~> maybe'title .= maybe_ (optField "title" schema)
    <~> maybe'summary .= maybe_ (optField "summary" schema)
    <~> maybe'image .= maybe_ (optField "image" assetSchema)

assetOriginalSchema :: ValueSchema NamedSwaggerDoc Asset'Original
assetOriginalSchema =
  object "Asset'Original" $
    pure Proto.defMessage
      <~> mimeType .= field "mimeType" schema
      <~> size .= field "size" schema
      <~> maybe'metaData .= maybe_ (optField "metaData" assetOriginalMetaDataSchema)
      <~> maybe'source .= maybe_ (optField "source" schema)
      <~> maybe'caption .= maybe_ (optField "caption" schema)

assetOriginalMetaDataSchema :: ValueSchema NamedSwaggerDoc Asset'Original'MetaData
assetOriginalMetaDataSchema =
  oneof "Asset'Original'MetaData" getAOMType $ \case
    AOMTypeImage -> tag _Asset'Original'Image assetImageMetaDataObjectSchema
    AOMTypeVideo -> tag _Asset'Original'Video assetVideoMetaDataObjectSchema
    AOMTypeAudio -> tag _Asset'Original'Audio assetAudioMetaDataObjectSchema

assetImageMetaDataObjectSchema :: ObjectSchema SwaggerDoc Asset'ImageMetaData
assetImageMetaDataObjectSchema =
  pure Proto.defMessage
    <~> width .= field "width" schema
    <~> height .= field "height" schema
    <~> maybe'tag .= maybe_ (optField "tag" schema)

assetVideoMetaDataObjectSchema :: ObjectSchema SwaggerDoc Asset'VideoMetaData
assetVideoMetaDataObjectSchema =
  pure Proto.defMessage
    <~> width .= field "width" schema
    <~> height .= field "height" schema
    <~> maybe'durationInMillis .= maybe_ (optField "durationInMillis" schema)

assetAudioMetaDataObjectSchema :: ObjectSchema SwaggerDoc Asset'AudioMetaData
assetAudioMetaDataObjectSchema =
  pure Proto.defMessage
    <~> maybe'durationInMillis .= maybe_ (optField "durationInMillis" schema)
    <~> maybe'normalizedLoudness .= maybe_ (optField "normalizedLoudness" bytestringSchema)

bytestringSchema :: ValueSchema NamedSwaggerDoc ByteString
bytestringSchema =
  Base64.encodeBase64 S..= parsedText "Base64ByteString" parseBS
  where
    parseBS :: Text -> Either String ByteString
    parseBS = first Text.unpack . Base64.decodeBase64 . Text.encodeUtf8

assetStatusSchema :: ValueSchema NamedSwaggerDoc Asset'Status
assetStatusSchema =
  oneof "Asset'Status" getASType $ \case
    ASTypeNotUploaded -> Prelude.id S..= field "notUploadedReason" (tag _Asset'NotUploaded assetNotUploadedSchema)
    ASTypeUploaded -> tag _Asset'Uploaded assetRemoteDataObjectSchema

assetNotUploadedSchema :: ValueSchema NamedSwaggerDoc Asset'NotUploaded
assetNotUploadedSchema =
  enum @Text "Asset'NotUploaded" $
    mconcat
      [ element "Cancelled" Asset'CANCELLED,
        element "Failed" Asset'FAILED
      ]

assetRemoteDataSchema :: ValueSchema NamedSwaggerDoc Asset'RemoteData
assetRemoteDataSchema = object "Asset'RemoteData" assetRemoteDataObjectSchema

assetRemoteDataObjectSchema :: ObjectSchema SwaggerDoc Asset'RemoteData
assetRemoteDataObjectSchema =
  pure Proto.defMessage
    <~> otrKey .= field "otrKey" bytestringSchema
    <~> sha256 .= field "sha256" bytestringSchema
    <~> maybe'assetId .= maybe_ (optField "assetId" schema)
    <~> maybe'assetToken .= maybe_ (optField "assetToken" schema)
    <~> maybe'encryption .= maybe_ (optField "encryption" encryptionAlgorithmSchema)

encryptionAlgorithmSchema :: ValueSchema NamedSwaggerDoc EncryptionAlgorithm
encryptionAlgorithmSchema =
  enum @Text "EncryptionAlgorithm" $
    mconcat
      [ element "AES_CBC" AES_CBC,
        element "AES_GCM" AES_GCM
      ]

assetPreviewSchema :: ValueSchema NamedSwaggerDoc Asset'Preview
assetPreviewSchema =
  object "Asset'Preview" $
    pure Proto.defMessage
      <~> mimeType .= field "mimeType" schema
      <~> size .= field "size" schema
      <~> maybe'remote .= maybe_ (optField "remote" assetRemoteDataSchema)
      <~> maybe'metaData .= maybe_ (optField "metaData" assetPreviewMetaDataSchema)

assetPreviewMetaDataSchema :: ValueSchema NamedSwaggerDoc Asset'Preview'MetaData
assetPreviewMetaDataSchema =
  oneof "Asset'Preview'MetaData" getAPMType $ \APMTypeImage ->
    tag _Asset'Preview'Image assetImageMetaDataObjectSchema

linkPreviewMetaDataSchema :: ValueSchema NamedSwaggerDoc LinkPreview'MetaData
linkPreviewMetaDataSchema =
  oneof "LinkPreview'MetaData" getLPMType $ \LPMTypeTweet ->
    tag _LinkPreview'Tweet tweetObjectSchema

tweetObjectSchema :: ObjectSchema SwaggerDoc Tweet
tweetObjectSchema =
  pure Proto.defMessage
    <~> maybe'author .= maybe_ (optField "author" schema)
    <~> maybe'username .= maybe_ (optField "username" schema)

mentionSchema :: ValueSchema NamedSwaggerDoc Mention
mentionSchema =
  object "Mention" $
    pure Proto.defMessage
      <~> start .= field "start" schema
      <~> M.length .= field "length" schema
      <~> maybe'mentionType .= maybe_ (optField "mentionType" mentionTypeSchema)

mentionTypeSchema :: ValueSchema NamedSwaggerDoc Mention'MentionType
mentionTypeSchema =
  oneof "Mention'MentionType" getMentionType $ \MentionTypeUserId ->
    tag _Mention'UserId (Prelude.id S..= field "userId" schema)

quoteSchema :: ValueSchema NamedSwaggerDoc Quote
quoteSchema =
  object "Quote" $
    pure Proto.defMessage
      <~> quotedMessageId .= field "quotedMessageId" schema
      <~> maybe'quotedMessageSha256 .= maybe_ (optField "quotedMessageSha256" bytestringSchema)

legalHoldStatusSchema :: ValueSchema NamedSwaggerDoc LegalHoldStatus
legalHoldStatusSchema =
  enum @Text "LegalHoldStatus" $
    mconcat
      [ element "UNKNOWN" UNKNOWN,
        element "DISABLED" DISABLED,
        element "ENABLED" ENABLED
      ]

-- * Helper types for oneof types

-- | GMC = GenericMessage'Content
data GMCType
  = GMCTypeText
  | GMCTypeImage
  | GMCTypeKnock
  | GMCTypeLastRead
  | GMCTypeCleared
  | GMCTypeExternal
  | GMCTypeClientAction
  | GMCTypeCalling
  | GMCTypeAsset
  | GMCTypeHidden
  | GMCTypeLocation
  | GMCTypeDeleted
  | GMCTypeEdited
  | GMCTypeConfirmation
  | GMCTypeReaction
  | GMCTypeEphemeral
  | GMCTypeAvailability
  | GMCTypeComposite
  | GMCTypeButtonAction
  | GMCTypeButtonActionConfirmation
  deriving (Show, Eq, Bounded, Enum)

getGMCType :: GenericMessage'Content -> GMCType
getGMCType = \case
  GenericMessage'Text _ -> GMCTypeText
  GenericMessage'Image _ -> GMCTypeImage
  GenericMessage'Knock _ -> GMCTypeKnock
  GenericMessage'LastRead _ -> GMCTypeLastRead
  GenericMessage'Cleared _ -> GMCTypeCleared
  GenericMessage'External _ -> GMCTypeExternal
  GenericMessage'ClientAction _ -> GMCTypeClientAction
  GenericMessage'Calling _ -> GMCTypeCalling
  GenericMessage'Asset _ -> GMCTypeAsset
  GenericMessage'Hidden _ -> GMCTypeHidden
  GenericMessage'Location _ -> GMCTypeLocation
  GenericMessage'Deleted _ -> GMCTypeDeleted
  GenericMessage'Edited _ -> GMCTypeEdited
  GenericMessage'Confirmation _ -> GMCTypeConfirmation
  GenericMessage'Reaction _ -> GMCTypeReaction
  GenericMessage'Ephemeral _ -> GMCTypeEphemeral
  GenericMessage'Availability _ -> GMCTypeAvailability
  GenericMessage'Composite _ -> GMCTypeComposite
  GenericMessage'ButtonAction _ -> GMCTypeButtonAction
  GenericMessage'ButtonActionConfirmation _ -> GMCTypeButtonActionConfirmation

instance ToSchema GMCType where
  schema =
    enum @Text "GMCType" $
      mconcat
        [ element "Text" GMCTypeText,
          element "Image" GMCTypeImage,
          element "Knock" GMCTypeKnock,
          element "LastRead" GMCTypeLastRead,
          element "Cleared" GMCTypeCleared,
          element "External" GMCTypeExternal,
          element "ClientAction" GMCTypeClientAction,
          element "Calling" GMCTypeCalling,
          element "Asset" GMCTypeAsset,
          element "Hidden" GMCTypeHidden,
          element "Location" GMCTypeLocation,
          element "Deleted" GMCTypeDeleted,
          element "Edited" GMCTypeEdited,
          element "Confirmation" GMCTypeConfirmation,
          element "Reaction" GMCTypeReaction,
          element "Ephemeral" GMCTypeEphemeral,
          element "Availability" GMCTypeAvailability,
          element "Composite" GMCTypeComposite,
          element "ButtonAction" GMCTypeButtonAction,
          element "ButtonActionConfirmation" GMCTypeButtonActionConfirmation
        ]

-- | LPP = LinkPreview'Preview
data LPPType = LPPTypeArticle
  deriving (Eq, Show, Bounded, Enum)

getLPPType :: LinkPreview'Preview -> LPPType
getLPPType (LinkPreview'Article _) = LPPTypeArticle

instance ToSchema LPPType where
  schema =
    enum @Text "LPPType" $
      mconcat [element "Article" LPPTypeArticle]

data LPMType = LPMTypeTweet
  deriving (Eq, Show, Bounded, Enum)

-- | LPM = LinkPreview'MetaData
getLPMType :: LinkPreview'MetaData -> LPMType
getLPMType (LinkPreview'Tweet _) = LPMTypeTweet

instance ToSchema LPMType where
  schema =
    enum @Text "LPMType" $
      mconcat [element "Tweet" LPMTypeTweet]

-- | AOM = Asset'Original'MetaData
data AOMType
  = AOMTypeImage
  | AOMTypeVideo
  | AOMTypeAudio
  deriving (Eq, Show, Bounded, Enum)

getAOMType :: Asset'Original'MetaData -> AOMType
getAOMType = \case
  Asset'Original'Image _ -> AOMTypeImage
  Asset'Original'Video _ -> AOMTypeVideo
  Asset'Original'Audio _ -> AOMTypeAudio

instance ToSchema AOMType where
  schema =
    enum @Text "AOMType" $
      mconcat
        [ element "Image" AOMTypeImage,
          element "Video" AOMTypeVideo,
          element "Audio" AOMTypeAudio
        ]

-- | AS = Asset'Status
data ASType
  = ASTypeNotUploaded
  | ASTypeUploaded
  deriving (Eq, Show, Bounded, Enum)

getASType :: Asset'Status -> ASType
getASType = \case
  Asset'NotUploaded _ -> ASTypeNotUploaded
  Asset'Uploaded _ -> ASTypeUploaded

instance ToSchema ASType where
  schema =
    enum @Text "ASType" $
      mconcat
        [ element "NotUploaded" ASTypeNotUploaded,
          element "Uploaded" ASTypeUploaded
        ]

-- | APM = Asset'Preview'MetaData
data APMType = APMTypeImage
  deriving (Eq, Show, Bounded, Enum)

getAPMType :: Asset'Preview'MetaData -> APMType
getAPMType (Asset'Preview'Image _) = APMTypeImage

instance ToSchema APMType where
  schema = enum @Text "APMType" $ mconcat [element "Image" APMTypeImage]

data MentionType = MentionTypeUserId
  deriving (Eq, Show, Bounded, Enum)

getMentionType :: Mention'MentionType -> MentionType
getMentionType (Mention'UserId _) = MentionTypeUserId

instance ToSchema MentionType where
  schema = enum @Text "MentionType" $ mconcat [element "UserId" MentionTypeUserId]

-- | EC = Ephmeral'Content
data ECType
  = ECTypeText
  | ECTypeImage
  | ECTypeKnock
  | ECTypeAsset
  | ECTypeLocation
  deriving (Eq, Show, Bounded, Enum)

getECType :: Ephemeral'Content -> ECType
getECType (Ephemeral'Text _) = ECTypeText
getECType (Ephemeral'Image _) = ECTypeImage
getECType (Ephemeral'Knock _) = ECTypeKnock
getECType (Ephemeral'Asset _) = ECTypeAsset
getECType (Ephemeral'Location _) = ECTypeLocation

instance ToSchema ECType where
  schema =
    enum @Text "ECType" $
      mconcat
        [ element "Text" ECTypeText,
          element "Image" ECTypeImage,
          element "Knock" ECTypeKnock,
          element "Asset" ECTypeAsset,
          element "Location" ECTypeLocation
        ]

-- | CIC = Composite'Item'Content
data CICType
  = CICTypeText
  | CICTypeButton
  deriving (Eq, Show, Bounded, Enum)

getCICType :: Composite'Item'Content -> CICType
getCICType = \case
  Composite'Item'Text _ -> CICTypeText
  Composite'Item'Button _ -> CICTypeButton

instance ToSchema CICType where
  schema =
    enum @Text "CICType" $
      mconcat
        [ element "Text" CICTypeText,
          element "Button" CICTypeButton
        ]

-- * Schema Profunctor meets Lenses

(<~>) :: Applicative f => f a -> f (a -> b) -> f b
(<~>) = flip (<*>)

infixl 8 <~>

(.=) ::
  forall a s.
  Lens' s a ->
  ObjectSchemaP SwaggerDoc a a ->
  ObjectSchemaP SwaggerDoc s (s -> s)
(.=) lens parserDoc = (lens .~) <$> view lens S..= parserDoc

oneof ::
  (Bounded typ, Enum typ, ToSchema typ) =>
  Text ->
  (a -> typ) ->
  (typ -> SchemaP SwaggerDoc Aeson.Object [Aeson.Pair] a b) ->
  SchemaP NamedSwaggerDoc Aeson.Value Aeson.Value a b
oneof objectName mkType f =
  dimap (\o -> (mkType o, o)) snd
    . object objectName
    $ bind
      (fst S..= field "type" schema)
      (snd S..= fieldOver _1 "data" (dispatch $ \t -> unnamed . object objectName $ f t))
