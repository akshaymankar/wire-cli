module Main where

import Wire.CLI.App
import Wire.CLI.Execute
import qualified Wire.CLI.Options as Opts

main :: IO ()
main = do
  Opts.RunConfig cfg (Opts.AnyCommand cmd) <- Opts.readOptions
  runApp cfg $ executeAndPrint cmd
