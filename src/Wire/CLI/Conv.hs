module Wire.CLI.Conv where

import Data.Id (ConvId)
import Data.Qualified
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Wire.API.Conversation hiding (Member)
import Wire.CLI.Backend (Backend)
import qualified Wire.CLI.Backend as Backend
import Wire.CLI.Error (WireCLIError)
import qualified Wire.CLI.Error as WireCLIError
import Wire.CLI.Store (Store)
import qualified Wire.CLI.Store as Store

sync :: Members '[Backend, Store, Error WireCLIError] r => Sem r ()
sync = do
  creds <- Store.getCreds >>= Error.note WireCLIError.NotLoggedIn
  convs <- getAllConvs $ Backend.listConvs creds 500
  Store.saveConvs convs

getAllConvs :: Member Backend r => (Maybe ConvId -> Sem r (ConversationList Conversation)) -> Sem r [Conversation]
getAllConvs f = loop Nothing
  where
    loop x = do
      (ConversationList convs hasMore) <- f x
      if hasMore
        then (convs <>) <$> loop (Just . qUnqualified $ cnvQualifiedId (last convs))
        else pure convs

list :: Members '[Store, Error WireCLIError] r => Sem r [Conversation]
list = do
  maybeConvs <- Store.getConvs
  case maybeConvs of
    Just convs -> pure convs
    Nothing -> Error.throw WireCLIError.NoConversationsFound
