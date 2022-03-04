module Wire.CLI.Display.Print where

import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UnagiNB
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO as Text
import Polysemy
import Wire.CLI.Display.Effect
import Wire.CLI.Notification.Types (ProcessedNotification)
import Wire.CLI.Chan (ReadChan)
import qualified Wire.CLI.Chan as Chan

run :: Members [ReadChan, Embed IO] r => Sem (Display ': r) a -> Sem r a
run = interpret $ \case
  ListConvs convs -> printJSON convs
  Search results -> printJSON results
  ListConnections conns -> printJSON conns
  ListMessages msgs -> printJSON msgs
  Login Nothing -> embed $ Text.putStrLn "Login successful!"
  Login (Just err) -> embed . Text.putStrLn $ "Login failed with error: " <> err
  ShowSelfUser u -> printJSON u
  ShowNotifications ns -> printJSON ns
  ShowNotificationsLive notifWatch -> printNotifsLive notifWatch

printJSON :: (Member (Embed IO) r, Aeson.ToJSON a) => a -> Sem r ()
printJSON = embed . LBS.putStrLn . Aeson.encode

printNotifsLive :: Members [ReadChan, Embed IO] r => UnagiNB.OutChan ProcessedNotification -> Sem r ()
printNotifsLive notifWatch = do
  eNotif <- Chan.readChan notifWatch
  case eNotif of
    Right n -> printJSON n >> printNotifsLive notifWatch
    Left _ -> pure ()
