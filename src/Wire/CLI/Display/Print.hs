module Wire.CLI.Display.Print where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Polysemy
import Wire.CLI.Display.Effect
import qualified Data.Text.IO as Text

run :: Member (Embed IO) r => Sem (Display ': r) a -> Sem r a
run = interpret $ \case
  ListConvs convs -> printJSON convs
  Search results -> printJSON results
  ListConnections conns -> printJSON conns
  ListMessages msgs -> printJSON msgs
  Login Nothing -> embed $ Text.putStrLn "Login successful!"
  Login (Just err) -> embed . Text.putStrLn $ "Login failed with error: " <> err

printJSON :: (Member (Embed IO) r, Aeson.ToJSON a) => a -> Sem r ()
printJSON = embed . LBS.putStrLn . Aeson.encode
