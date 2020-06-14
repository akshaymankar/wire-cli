{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Mocks.Store where

import Test.Polysemy.Mock.TH
import Wire.CLI.Store (Store)

genMock ''Store
