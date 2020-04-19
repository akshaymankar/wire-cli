{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Wire.CLI.MockCryptoBox
  ( MockCryptoBox,
    CryptoBoxC,
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

import Control.Algebra
import Control.Carrier.State.Strict (StateC, evalState, get, put)
import Data.Word
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

mockRandomBytes :: Has MockCryptoBox sig m => Word32 -> m (CBox.Result [Word8])
mockRandomBytes = send . MockRandomBytes

mockRandomBytesCalls :: Has MockCryptoBox sig m => m [Word32]
mockRandomBytesCalls = send MockRandomBytesCalls

mockRandomBytesReturns :: Has MockCryptoBox sig m => (Word32 -> CBox.Result [Word8]) -> m ()
mockRandomBytesReturns = send . MockRandomBytesReturns

mockNewPrekey :: Has MockCryptoBox sig m => Word16 -> m (CBox.Result Prekey)
mockNewPrekey = send . MockNewPrekey

mockNewPrekeyCalls :: Has MockCryptoBox sig m => m [Word16]
mockNewPrekeyCalls = send MockNewPrekeyCalls

mockNewPrekeyReturns :: Has MockCryptoBox sig m => (Word16 -> CBox.Result Prekey) -> m ()
mockNewPrekeyReturns = send . MockNewPrekeyReturns

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

mockToState :: (Algebra sig m) => MockCryptoBox n a -> StateC MockCryptoBoxState m a
mockToState = \case
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
    (newPrekeyCalls <$> get)
  MockNewPrekeyReturns f -> do
    state <- get
    put state {newPrekeyReturns = f}

interpretToMock :: (Has MockCryptoBox sig m2) => CryptoBox m1 a -> m2 a
interpretToMock = \case
  CryptoBox.RandomBytes n -> mockRandomBytes n
  CryptoBox.NewPrekey n -> mockNewPrekey n

run :: Functor m => MockCryptoBoxC m a -> m a
run = evalState initialMockCryptoBoxState . runMockCryptoBoxC

newtype MockCryptoBoxC m a = MockCryptoBoxC {runMockCryptoBoxC :: StateC MockCryptoBoxState m a}
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (MockCryptoBox :+: sig) (MockCryptoBoxC m) where
  alg hdl sig ctx =
    MockCryptoBoxC $
      case sig of
        L c -> (<$ ctx) <$> mockToState c
        R other -> alg (runMockCryptoBoxC . hdl) (R other) ctx

newtype CryptoBoxC m a = CryptoBoxC {mock :: m a}
  deriving (Functor, Applicative, Monad)

instance (Has MockCryptoBox sig m, Algebra sig m) => Algebra (CryptoBox :+: sig) (CryptoBoxC m) where
  alg hdl sig ctx =
    CryptoBoxC $
      case sig of
        L c -> (<$ ctx) <$> interpretToMock c
        R other -> alg (mock . hdl) other ctx
