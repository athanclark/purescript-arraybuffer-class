module Test.Main where

import Data.ArrayBuffer.Class


import Prelude
import Data.Maybe (Maybe (..))
import Data.UInt as UInt
import Data.UInt.Gen (genUInt)
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (Result (Failed), (===), quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Math (pow)



genU :: Number -> Gen UInt.UInt
genU high = genUInt (UInt.fromInt 0) (UInt.fromNumber high)

genUint8 :: Gen UInt.UInt
genUint8 = genU ((pow 2.0 8.0) - 1.0)

genUint16 :: Gen UInt.UInt
genUint16 = genU ((pow 2.0 16.0) - 1.0)

genUint32 :: Gen UInt.UInt
genUint32 = genU ((pow 2.0 32.0) - 1.0)


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




arrayBufferIso :: forall a. Show a => Eq a => EncodeArrayBuffer a => DecodeArrayBuffer a => a -> Result
arrayBufferIso x = unsafePerformEffect do
  b <- encodeArrayBuffer x
  mY <- decodeArrayBuffer b
  case mY of
    Nothing -> pure (Failed "Nothing")
    Just y -> pure (x === y)
