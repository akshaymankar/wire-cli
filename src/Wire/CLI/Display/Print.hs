module Wire.CLI.Display.Print where

import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8 as LBS
import Polysemy
import Wire.CLI.Display.Effect

run :: Member (Embed IO) r => Sem (Display ': r) a -> Sem r a
run = interpret $ \case
  ListConvs convs -> printJSON convs
  Search results -> printJSON results
  ListConnections conns -> printJSON conns
  ListMessages msgs -> printJSON msgs

printJSON :: (Member (Embed IO) r, Aeson.ToJSON a) => a -> Sem r ()
printJSON = embed . LBS.putStrLn . Aeson.encode
