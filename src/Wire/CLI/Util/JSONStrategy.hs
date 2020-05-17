module Wire.CLI.Util.JSONStrategy
  ( JSONStrategy,
    module Deriving.Aeson,
  )
where

import Deriving.Aeson

type JSONStrategy prefix =
  CustomJSON
    '[ FieldLabelModifier (StripPrefix prefix, CamelToSnake),
       OmitNothingFields
     ]
