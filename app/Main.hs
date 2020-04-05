module Main where

import Wire.CLI.Options

main :: IO ()
main = readOptions >>= print
