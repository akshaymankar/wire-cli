{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Mocks.UUIDGen where

import Test.Polysemy.Mock.TH (genMock)
import Wire.CLI.UUIDGen (UUIDGen)

genMock ''UUIDGen
