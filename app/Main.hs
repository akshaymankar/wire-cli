module Main where

import Wire.CLI.App
import Wire.CLI.Execute
import Wire.CLI.Options

main :: IO ()
main = do
  opts <- readOptions
  runApp $ execute opts
