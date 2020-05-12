module Main where

import Wire.CLI.App
import qualified Wire.CLI.Display as Display
import Wire.CLI.Execute
import Wire.CLI.Options

main :: IO ()
main = do
  opts <- readOptions $ Handlers Display.listConvs
  runApp $ execute opts
