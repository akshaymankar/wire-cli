{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Mocks.CryptoBox where

import Test.Polysemy.Mock.TH
import Wire.CLI.CryptoBox (CryptoBox)

genMock ''CryptoBox
