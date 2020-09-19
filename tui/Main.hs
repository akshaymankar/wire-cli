{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Wire.TUI.Main

main :: IO ()
main = do
  defaultMain app (State "")
  pure ()
