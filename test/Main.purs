module Test.Main where

import Data.ArrayBuffer.Class
import Data.ArrayBuffer.Typed.Gen
  (genUint8, genUint16, genUint32, genInt8, genInt16, genInt32, genFloat32, genFloat64)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UInt as UInt
import Data.UInt.Gen (genUInt)
import Data.Float32.Gen (chooseFloat32)
import Data.Int (floor)
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (Result (Failed), (===), quickCheckGen)
import Test.QuickCheck.Gen (Gen, chooseInt, choose)
import Math (pow)



main :: Effect Unit
main = do
  log "Running tests..."
  log "  Uint32BE:"
  quickCheckGen ((arrayBufferIso <<< Uint32BE) <$> genUint32)
  log "  Uint32LE:"
  quickCheckGen ((arrayBufferIso <<< Uint32LE) <$> genUint32)
  log "  Uint16BE:"
  quickCheckGen ((arrayBufferIso <<< Uint16BE) <$> genUint16)
  log "  Uint16LE:"
  quickCheckGen ((arrayBufferIso <<< Uint16LE) <$> genUint16)
  log "  Uint8:"
  quickCheckGen ((arrayBufferIso <<< Uint8) <$> genUint8)
  log "  Int32BE:"
  quickCheckGen ((arrayBufferIso <<< Int32BE) <$> genInt32)
  log "  Int32LE:"
  quickCheckGen ((arrayBufferIso <<< Int32LE) <$> genInt32)
  log "  Int16BE:"
  quickCheckGen ((arrayBufferIso <<< Int16BE) <$> genInt16)
  log "  Int16LE:"
  quickCheckGen ((arrayBufferIso <<< Int16LE) <$> genInt16)
  log "  Int8:"
  quickCheckGen ((arrayBufferIso <<< Int8) <$> genInt8)
  log "  Float32BE:"
  quickCheckGen ((arrayBufferIso <<< Float32BE) <$> genFloat32)
  log "  Float32LE:"
  quickCheckGen ((arrayBufferIso <<< Float32LE) <$> genFloat32)
  log "  Float64BE:"
  quickCheckGen ((arrayBufferIso <<< Float64BE) <$> genFloat64)
  log "  Float64LE:"
  quickCheckGen ((arrayBufferIso <<< Float64LE) <$> genFloat64)




arrayBufferIso :: forall a. Show a => Eq a => EncodeArrayBuffer a => DecodeArrayBuffer a => a -> Result
arrayBufferIso x = unsafePerformEffect do
  b <- encodeArrayBuffer x
  mY <- decodeArrayBuffer b
  case mY of
    Nothing -> pure (Failed "Nothing")
    Just y -> pure (x === y)
