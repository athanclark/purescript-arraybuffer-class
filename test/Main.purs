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
import Data.Set (Set, fromFoldable) as Set
import Data.Map (Map, fromFoldable) as Map
import Foreign.Object (Object, fromFoldable) as O
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck
  (Result (Failed), (===), quickCheckGen, quickCheckGen', quickCheck, quickCheck', arbitrary)
import Test.QuickCheck.Gen (Gen)



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
  quickCheck (arrayBufferIso :: Maybe String -> Result)
  log "  Tuple"
  quickCheck (arrayBufferIso :: Tuple String String -> Result)
  log "  Either"
  quickCheck (arrayBufferIso :: Either String String -> Result)
  log "  Array"
  quickCheck' 1000 (arrayBufferIso :: Array String -> Result)
  log "  List"
  quickCheck' 1000 (arrayBufferIso :: List String -> Result)
  log "  NonEmpty Array"
  quickCheck' 1000 (arrayBufferIso :: NonEmpty Array String -> Result)
  log "  NonEmpty List"
  quickCheck' 1000 (arrayBufferIso :: NonEmpty List String -> Result)

  log "  Object"
  quickCheckGen' 1000 (arrayBufferIso <$> genObject)
  log "  Set"
  quickCheckGen' 1000 (arrayBufferIso <$> genSet)
  log "  Map"
  quickCheckGen' 1000 (arrayBufferIso <$> genMap)



genObject :: Gen (O.Object String)
genObject = do
  (xs :: Array (Tuple String String)) <- arbitrary
  pure (O.fromFoldable xs)

genSet :: Gen (Set.Set String)
genSet = do
  (xs :: Array String) <- arbitrary
  pure (Set.fromFoldable xs)

genMap :: Gen (Map.Map String String)
genMap = do
  (xs :: Array (Tuple String String)) <- arbitrary
  pure (Map.fromFoldable xs)


arrayBufferIso :: forall a. Show a => Eq a => EncodeArrayBuffer a => DecodeArrayBuffer a => a -> Result
arrayBufferIso x = unsafePerformEffect do
  b <- encodeArrayBuffer x
  mY <- decodeArrayBuffer b
  case mY of
    Nothing -> pure (Failed "Nothing")
    Just y -> pure (x === y)
