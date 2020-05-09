{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Mocks where

import Test.Polysemy.Mock.TH
import Wire.CLI.Backend (Backend)
import Wire.CLI.Backend.Arbitrary ()
import Wire.CLI.CryptoBox (CryptoBox)
import Wire.CLI.Store (Store)

genMock ''Backend

genMock ''Store

genMock ''CryptoBox
