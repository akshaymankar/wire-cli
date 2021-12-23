{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Backend.API where

import Data.ByteString.Conversion
import Data.CommaSeparatedList
import Data.Misc
import Data.Proxy
import Data.Range
import qualified Data.Set as Set
import Data.Text
import qualified Data.Text.Encoding as Text
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic
import Wire.API.ErrorDescription
import Wire.API.Message
import Wire.API.Routes.Public
import qualified Wire.API.Routes.Public.Brig as Brig
import qualified Wire.API.Routes.Public.Galley as Galley
import Wire.API.ServantProto

brigClient :: Brig.Api (AsClientT ClientM)
brigClient = genericClient

galleyClient :: Galley.Api (AsClientT ClientM)
galleyClient = genericClient

-- * Orphan instances which should be in wire-api

instance ToHttpApiData a => ToHttpApiData (Range n m a) where
  toUrlPiece = toUrlPiece . fromRange

instance ToByteString (List a) => ToHttpApiData (CommaSeparatedList a) where
  toUrlPiece = Text.decodeUtf8 . toByteString' . List . fromCommaSeparatedList

instance ToHttpApiData IgnoreMissing where
  toUrlPiece = \case
    IgnoreMissingAll -> "true"
    IgnoreMissingList uids ->
      if Set.null uids
        then "false"
        else toUrlPiece . CommaSeparatedList $ Set.toList uids

instance ToHttpApiData ReportMissing where
  toUrlPiece = \case
    ReportMissingAll -> "true"
    ReportMissingList uids ->
      if Set.null uids
        then "false"
        else toUrlPiece . CommaSeparatedList $ Set.toList uids

-- TODO: Fix this in brig API, and write a special 'HasClient' which ignores the
-- 'X-Forwarded-For' header
instance ToHttpApiData IpAddr where
  toUrlPiece _ =
    "127.0.0.1"

instance (RunClient m, HasClient m api) => HasClient m (ZUser :> api) where
  type Client m (ZUser :> api) = Text -> Client m api
  clientWithRoute proxyM _proxyZ req token =
    clientWithRoute proxyM (Proxy @api) (addHeader "Authorization" ("Bearer " <> token) req)
  hoistClientMonad proxyM _proxyZ f cl token =
    hoistClientMonad proxyM (Proxy @api) f (cl token)

instance (RunClient m, HasClient m api) => HasClient m (ZLocalUser :> api) where
  type Client m (ZLocalUser :> api) = Text -> Client m api
  clientWithRoute proxyM _proxyZ req token =
    clientWithRoute proxyM (Proxy @api) (addHeader "Authorization" ("Bearer " <> token) req)
  hoistClientMonad proxyM _proxyZ f cl token =
    hoistClientMonad proxyM (Proxy @api) f (cl token)

-- Ignores ZConn, is this right?
instance (RunClient m, HasClient m api) => HasClient m (ZConn :> api) where
  type Client m (ZConn :> api) = Client m api
  clientWithRoute proxyM _proxyZ req =
    clientWithRoute proxyM (Proxy @api) req
  hoistClientMonad proxyM _proxyZ f cl =
    hoistClientMonad proxyM (Proxy @api) f cl

instance (RunClient m, HasClient m api) => HasClient m (CanThrow err :> api) where
  type Client m (CanThrow err :> api) = Client m api
  clientWithRoute proxyM _proxyZ req =
    clientWithRoute proxyM (Proxy @api) req
  hoistClientMonad proxyM _proxyZ f cl =
    hoistClientMonad proxyM (Proxy @api) f cl

instance ToProto (RawProto a) where
  toProto = rpRaw

instance ToProto a => MimeRender Proto a where
  mimeRender _proxy = toProto
