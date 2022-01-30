{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Wire.CLI.Notification.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Profunctor (dimap)
import Data.ProtoLens.Prism (Prism', prism')
import Data.Qualified
import Data.Schema
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Wire.CLI.Backend.Event as Event
import qualified Wire.CLI.Store.StoredMessage as Store
import Wire.CLI.Util.Schema (toObjectSchema)

data ProcessedNotification
  = DecryptedMessage (Qualified ConvId) Store.StoredMessage
  | PlainNotification Event.ExtensibleEvent
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via Schema ProcessedNotification

_DecryptedMessage :: Prism' ProcessedNotification (Qualified ConvId, Store.StoredMessage)
_DecryptedMessage = prism' (uncurry DecryptedMessage) $ \case
  DecryptedMessage qual sm -> Just (qual, sm)
  PlainNotification _ -> Nothing

_PlainNotification :: Prism' ProcessedNotification Event.ExtensibleEvent
_PlainNotification = prism' PlainNotification $ \case
  DecryptedMessage _ _ -> Nothing
  PlainNotification ev -> Just ev

instance ToSchema ProcessedNotification where
  schema =
    object "ProcessedNotification" $
      dimap (\o -> (getPNType o, o)) snd $
        bind
          (fst .= field "type" schema)
          ( snd
              .= dispatch
                ( \case
                    PNDecryptedMessage -> tag _DecryptedMessage decyptedMessageObjetSchema
                    PNPlain -> tag _PlainNotification (toObjectSchema schema)
                )
          )

decyptedMessageObjetSchema :: ObjectSchema SwaggerDoc (Qualified ConvId, Store.StoredMessage)
decyptedMessageObjetSchema =
  (,)
    <$> fst .= field "conv" schema
    <*> snd .= field "message" schema

getPNType :: ProcessedNotification -> PNType
getPNType = \case
  DecryptedMessage _ _ -> PNDecryptedMessage
  PlainNotification _ -> PNPlain

data PNType = PNDecryptedMessage | PNPlain
  deriving (Show, Eq, Bounded, Enum)

instance ToSchema PNType where
  schema =
    enum @Text "PNType" $
      mconcat
        [ element "decrypted_message" PNDecryptedMessage,
          element "plain" PNPlain
        ]
