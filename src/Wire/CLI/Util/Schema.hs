{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.CLI.Util.Schema where

import Data.Aeson (FromJSON, ToJSON)
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM

newtype NoDoc a = NoDoc {unNoDoc :: a}
  deriving newtype (FromJSON, ToJSON)

instance (FromJSON a, ToJSON a) => ToSchema (NoDoc a) where
  schema = genericToSchema

instance S.ToSchema (NoDoc a) where
  declareNamedSchema _ = pure $ S.NamedSchema Nothing mempty

toObjectSchema :: ValueSchema NamedSwaggerDoc a -> ObjectSchema SwaggerDoc a
toObjectSchema sch = mkSchema d r w
  where
    d = schemaDoc $ unnamed sch
    r = schemaIn sch . Aeson.Object
    w x = case schemaOut sch x of
      Just (Aeson.Object o) -> Just $ KM.toList o
      _ -> Nothing
