{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.CLI.Mocks.Backend where

import Test.Polysemy.Mock.TH
import Wire.CLI.Backend (Backend)

genMock ''Backend