module Main where

import Wire.CLI.App
import qualified Wire.CLI.Display as Display
import Wire.CLI.Execute
import qualified Wire.CLI.Options as Opts

main :: IO ()
main = do
  Opts.RunConfig cfg cmd <- Opts.readOptions $ Opts.Handlers Display.listConvs Display.search
  runApp cfg $ execute cmd
