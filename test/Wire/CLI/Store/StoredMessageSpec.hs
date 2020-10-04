module Wire.CLI.Store.StoredMessageSpec where

import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson.Types as Aeson
import Data.Either (isLeft)
import qualified Data.ProtoLens as Proto
import qualified Proto.Messages as M
import Test.Hspec (Spec, describe, focus)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))
import Test.QuickCheck.Property ((==>))
import qualified Wire.CLI.Store as Store
import Wire.CLI.Store.Arbitrary ()

spec :: Spec
spec = describe "Wire.CLI.Store.StoredMessage" $ do
  describe "decodeMessage" $ do
    prop "decoding any known valid message results in a ValidMessage" $
      \msg -> Store.decodeMessage (Proto.encodeMessage msg) === Store.ValidMessage msg

    prop "decoding any known invalid message results in an InvalidMessage" $
      let isInvalid = \case
            Store.InvalidMessage _ -> True
            _ -> False
       in \msg ->
            isLeft (Proto.decodeMessage @M.GenericMessage msg)
              ==> isInvalid (Store.decodeMessage msg)
  describe "To/FromJSON" $ do
    prop "roundtrips" $ do
      \(msg :: Store.StoredMessageData) -> Aeson.parseEither Aeson.parseJSON (toJSON msg) === Right msg
