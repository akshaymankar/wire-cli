module Wire.CLI.Util.JSONStrategy
  ( JSONStrategy,
    module Deriving.Aeson,
  )
where

import Deriving.Aeson

type JSONStrategy =
  CustomJSON
    '[ FieldLabelModifier (CamelToSnake, (TypToType, ClasToClass)),
       OmitNothingFields
     ]

data TypToType

instance StringModifier TypToType where
  getStringModifier "typ" = "type"
  getStringModifier n = n

data ClasToClass

instance StringModifier ClasToClass where
  getStringModifier "clas" = "class"
  getStringModifier n = n
