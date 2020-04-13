{-# LANGUAGE TemplateHaskell #-}

module Wire.CLI.MockCryptoBox
  ( MockCryptoBox,
    mockRandomBytes,
    mockRandomBytesCalls,
    mockRandomBytesReturns,
    mock,
    mockNewPrekey,
    mockNewPrekeyCalls,
    mockNewPrekeyReturns,
    Wire.CLI.MockCryptoBox.run,
  )
where

import Data.Word
import Polysemy
import Polysemy.State
import qualified System.CryptoBox as CBox
import Wire.CLI.Backend.Prekey (Prekey)
import Wire.CLI.CryptoBox (CryptoBox)
import qualified Wire.CLI.CryptoBox as CryptoBox

data MockCryptoBox m a where
  MockRandomBytes :: Word32 -> MockCryptoBox m (CBox.Result [Word8])
  MockRandomBytesCalls :: MockCryptoBox m [Word32]
  MockRandomBytesReturns :: (Word32 -> CBox.Result [Word8]) -> MockCryptoBox m ()
  MockNewPrekey :: Word16 -> MockCryptoBox m (CBox.Result Prekey)
  MockNewPrekeyCalls :: MockCryptoBox m [Word16]
  MockNewPrekeyReturns :: (Word16 -> CBox.Result Prekey) -> MockCryptoBox m ()

makeSem ''MockCryptoBox

data MockCryptoBoxState
  = MockCryptoBoxState
      { randomBytesCalls :: [Word32],
        randomBytesReturns :: Word32 -> CBox.Result [Word8],
        newPrekeyCalls :: [Word16],
        newPrekeyReturns :: Word16 -> CBox.Result Prekey
      }

initialMockCryptoBoxState :: MockCryptoBoxState
initialMockCryptoBoxState =
  MockCryptoBoxState
    { randomBytesCalls = [],
      randomBytesReturns = const (error "randomBytes not mocked yet"),
      newPrekeyCalls = [],
      newPrekeyReturns = const (error "newPrekey not mocked yet")
    }

run :: Sem (MockCryptoBox ': r) a -> Sem r a
run = evalState initialMockCryptoBoxState . mockCryptoBoxToState

mockCryptoBoxToState :: forall r a. Sem (MockCryptoBox ': r) a -> Sem (State MockCryptoBoxState ': r) a
mockCryptoBoxToState = reinterpret $ \case
  MockRandomBytes n -> do
    state <- get
    put (state {randomBytesCalls = randomBytesCalls state <> [n]})
    pure $ randomBytesReturns state n
  MockRandomBytesCalls ->
    randomBytesCalls <$> get
  MockRandomBytesReturns f -> do
    state <- get
    put $ state {randomBytesReturns = f}
  MockNewPrekey n -> do
    state <- get
    put (state {newPrekeyCalls = newPrekeyCalls state <> [n]})
    pure $ newPrekeyReturns state n
  MockNewPrekeyCalls ->
    newPrekeyCalls <$> get
  MockNewPrekeyReturns f -> do
    state <- get
    put $ state {newPrekeyReturns = f}

mock :: Member MockCryptoBox r => Sem (CryptoBox ': r) a -> Sem r a
mock = interpret $ \case
  CryptoBox.RandomBytes n -> mockRandomBytes n
  CryptoBox.NewPrekey n -> mockNewPrekey n
