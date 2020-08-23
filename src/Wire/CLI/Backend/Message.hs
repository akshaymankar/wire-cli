{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Backend.Message where

import Data.Aeson (ToJSON (..), Value (..))
import Data.Coerce (coerce)
import Data.Key (FoldableWithKey (..), Key, Keyed (..), TraversableWithKey (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import Wire.CLI.Backend.Client (ClientId)
import Wire.CLI.Backend.Prekey (Prekey)
import Wire.CLI.Backend.User (UserId)
import Wire.CLI.Util.ByteStringJSON (Base64ByteString)
import Wire.CLI.Util.JSONStrategy

data NewOtrMessage = NewOtrMessage
  { nomNativePriority :: Maybe NativePriority,
    nomReportMissing :: Maybe [UserId],
    nomData :: Maybe Text,
    nomNativePush :: Maybe Bool,
    nomSender :: ClientId,
    nomTransient :: Maybe Bool,
    nomRecipients :: Recipients
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON) via JSONStrategy "nom" NewOtrMessage

mkNewOtrMessage :: ClientId -> Recipients -> NewOtrMessage
mkNewOtrMessage client rcpts = NewOtrMessage Nothing Nothing Nothing Nothing client Nothing rcpts

data NativePriority
  = NativePriorityHigh
  | NativePriorityLow
  deriving stock (Show, Eq)

instance ToJSON NativePriority where
  toJSON = \case
    NativePriorityLow -> String "low"
    NativePriorityHigh -> String "high"

newtype Recipients = Recipients {recipients :: UserClientMap Base64ByteString}
  deriving stock (Show, Eq)
  deriving newtype (ToJSON)

newtype UserClientMap a = UserClientMap {userClientMap :: Map UserId (Map ClientId a)}
  deriving stock (Show, Eq, Functor, Foldable, Traversable)
  deriving newtype (ToJSON, FromJSON, Semigroup, Monoid)

type instance Key UserClientMap = (UserId, ClientId)

instance Keyed UserClientMap where
  mapWithKey f = coerce $ Map.mapWithKey (\u -> Map.mapWithKey (curry f u))

instance FoldableWithKey UserClientMap where
  foldrWithKey f b = foldrWithKey (\u -> flip $ foldrWithKey (curry f u)) b . userClientMap

instance TraversableWithKey UserClientMap where
  traverseWithKey f =
    fmap UserClientMap
      . traverseWithKey (\u -> traverseWithKey (curry f u))
      . userClientMap

data SendOtrMessageResponse
  = OtrMessageResponseSuccess
  | OtrMessageResponseClientMismatch ClientMismatch
  deriving (Show)

data ClientMismatch = ClientMismatch
  { cmRedundant :: UserClients,
    cmTime :: UTCTime,
    cmMissing :: UserClients,
    cmDeleted :: UserClients
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via JSONStrategy "cm" ClientMismatch

newtype UserClients = UserClients (Map UserId [ClientId])
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

newtype PrekeyBundles = PrekeyBundles {prekeyBundles :: UserClientMap Prekey}
  deriving newtype (FromJSON)
