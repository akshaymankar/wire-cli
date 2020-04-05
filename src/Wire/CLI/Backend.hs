{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.Backend
  ( login,
    Backend (..),
    LoginResponse (..),
  )
where

import Data.Text
import Polysemy
import Wire.CLI.Options

data LoginResponse
  = LoginSuccess Text
  | LoginFailure Text

data Backend m a where
  Login :: LoginOptions -> Backend m LoginResponse

makeSem ''Backend
