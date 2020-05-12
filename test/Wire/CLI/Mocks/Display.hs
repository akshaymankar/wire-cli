{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Mocks.Display where

import Test.Polysemy.Mock.TH
import Wire.CLI.Display (Display)

genMock ''Display
