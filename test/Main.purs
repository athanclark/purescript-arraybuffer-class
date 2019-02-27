module Test.Main where

import Data.ArrayBuffer.Class
import Data.ArrayBuffer.Typed.Gen
  (genUint8, genUint16, genUint32, genInt8, genInt16, genInt32, genFloat32, genFloat64)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple)
import Data.Either (Either)
import Data.List (List)
import Data.NonEmpty (NonEmpty)
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (Result (Failed), (===), quickCheckGen, quickCheckGen', quickCheck, quickCheck')



main :: Effect Unit
main = do
  log "Running tests..."
  log "  Uint32BE:"
  quickCheckGen' 1000 ((arrayBufferIso <<< Uint32BE) <$> genUint32)
  log "  Uint32LE:"
  quickCheckGen' 1000 ((arrayBufferIso <<< Uint32LE) <$> genUint32)
  log "  Uint16BE:"
  quickCheckGen ((arrayBufferIso <<< Uint16BE) <$> genUint16)
  log "  Uint16LE:"
  quickCheckGen ((arrayBufferIso <<< Uint16LE) <$> genUint16)
  log "  Uint8:"
  quickCheckGen ((arrayBufferIso <<< Uint8) <$> genUint8)
  log "  Int32BE:"
  quickCheckGen' 1000 ((arrayBufferIso <<< Int32BE) <$> genInt32)
  log "  Int32LE:"
  quickCheckGen' 1000 ((arrayBufferIso <<< Int32LE) <$> genInt32)
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
  quickCheckGen' 1000 ((arrayBufferIso <<< Float64BE) <$> genFloat64)
  log "  Float64LE:"
  quickCheckGen' 1000 ((arrayBufferIso <<< Float64LE) <$> genFloat64)

  log "  Unit"
  quickCheck (arrayBufferIso :: Unit -> Result)
  log "  Boolean"
  quickCheck (arrayBufferIso :: Boolean -> Result)
  log "  Ordering"
  quickCheck (arrayBufferIso :: Ordering -> Result)
  log "  Char"
  quickCheck' 1000 (arrayBufferIso :: Char -> Result)
  log "  String"
  quickCheck' 1000 (arrayBufferIso :: String -> Result)

  log "  Maybe"
  quickCheck (arrayBufferIso :: Maybe Char -> Result)
  log "  Tuple"
  quickCheck (arrayBufferIso :: Tuple Char Char -> Result)
  log "  Either"
  quickCheck (arrayBufferIso :: Either Char Char -> Result)
  log "  Array"
  quickCheck' 1000 (arrayBufferIso :: Array Char -> Result)
  log "  List"
  quickCheck' 1000 (arrayBufferIso :: List Char -> Result)
  log "  NonEmpty Array"
  quickCheck' 1000 (arrayBufferIso :: NonEmpty Array Char -> Result)
  log "  NonEmpty List"
  quickCheck' 1000 (arrayBufferIso :: NonEmpty List Char -> Result)



arrayBufferIso :: forall a. Show a => Eq a => EncodeArrayBuffer a => DecodeArrayBuffer a => a -> Result
arrayBufferIso x = unsafePerformEffect do
  b <- encodeArrayBuffer x
  mY <- decodeArrayBuffer b
  case mY of
    Nothing -> pure (Failed "Nothing")
    Just y -> pure (x === y)
