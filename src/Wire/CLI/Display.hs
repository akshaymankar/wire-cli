{-# LANGUAGE NoImplicitPrelude #-}

module Wire.CLI.Display
  ( Display (..),
    listConvs,
    search,
    listConnections,
    listMessages,
    login,
    showSelfUser,
    showNotifications
  )
where

import Wire.CLI.Display.Effect
