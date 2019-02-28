module Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, putArrayBuffer
  , class DecodeArrayBuffer, readArrayBuffer
  , class DynamicByteLength, byteLength
  , encodeArrayBuffer
  , decodeArrayBuffer
  , module Data.ArrayBuffer.Class.Types
  , class GEncodeArrayBuffer
  , gPutArrayBuffer
  , class GDecodeArrayBuffer
  , gReadArrayBuffer
  ) where

import Data.ArrayBuffer.Class.Types
import Data.ArrayBuffer.Types
  (ArrayBuffer, ByteOffset, ByteLength, DataView, ArrayView, Uint8Array)
import Data.ArrayBuffer.ArrayBuffer (empty, byteLength, slice) as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (buffer, whole, traverse_, class TypedArray) as TA
import Data.ArrayBuffer.Typed.Unsafe (AV (..))

import Prelude
  ( Unit, Ordering (..), class Ord
  , (<$>), (<*>), (>>=), (<<<), (<=), (<>), (+), (-), (/=), (==), (<)
  , pure, otherwise, show, unit, bind, map, discard)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.List (List (..))
import Data.Void (Void)
import Data.NonEmpty (NonEmpty (..))
import Data.Monoid.Additive (Additive (..))
import Data.Monoid.Multiplicative (Multiplicative (..))
import Data.Monoid.Conj (Conj (..))
import Data.Monoid.Disj (Disj (..))
import Data.Monoid.Dual (Dual (..))
import Data.Monoid.Endo (Endo (..))
import Data.Set (Set, fromFoldable, toUnfoldable) as Set
import Data.Map (Map, fromFoldable, toUnfoldable) as Map
import Data.Hashable (class Hashable)
import Data.HashSet (HashSet, fromFoldable, toArray) as HS
import Data.HashMap (HashMap, toArrayBy, fromArray) as HM
import Data.Array (fromFoldable, toUnfoldable, cons, uncons) as Array
import Data.Enum (toEnum, fromEnum)
import Data.Traversable (for_, traverse)
import Data.Foldable (sum, length)
import Data.Unfoldable (replicateA)
import Data.UInt (fromInt, toInt, fromNumber, toNumber) as UInt
import Data.String (toCodePointArray, fromCodePointArray)
import Data.String.CodePoints (CodePoint, codePointFromChar, singleton)
import Data.String.CodeUnits (toChar)
import Data.Int.Bits ((.|.), (.&.), shr, shl, xor)
import Data.Symbol (class IsSymbol, SProxy (..), reflectSymbol)
import Foreign.Object (Object, toAscUnfoldable, fromFoldable, lookup, insert, empty) as O
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (kind RowList, Cons, Nil, class RowToList) as RL
import Record (insert, get) as Record
import Type.Data.RowList (RLProxy (..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (new, read, write, modify) as Ref
import Unsafe.Coerce (unsafeCoerce)


class DynamicByteLength a where
  -- | Get the byte length of a value at runtime. Should be identical for read and written values.
  byteLength :: a -> Effect ByteLength

instance dynamicByteLengthUint32BE :: DynamicByteLength Uint32BE where
  byteLength _ = pure 4
instance dynamicByteLengthUint32LE :: DynamicByteLength Uint32LE where
  byteLength _ = pure 4
instance dynamicByteLengthUint16BE :: DynamicByteLength Uint16BE where
  byteLength _ = pure 2
instance dynamicByteLengthUint16LE :: DynamicByteLength Uint16LE where
  byteLength _ = pure 2
instance dynamicByteLengthUint8 :: DynamicByteLength Uint8 where
  byteLength _ = pure 1
instance dynamicByteLengthInt32BE :: DynamicByteLength Int32BE where
  byteLength _ = pure 4
instance dynamicByteLengthInt32LE :: DynamicByteLength Int32LE where
  byteLength _ = pure 4
instance dynamicByteLengthInt16BE :: DynamicByteLength Int16BE where
  byteLength _ = pure 2
instance dynamicByteLengthInt16LE :: DynamicByteLength Int16LE where
  byteLength _ = pure 2
instance dynamicByteLengthInt8 :: DynamicByteLength Int8 where
  byteLength _ = pure 1
instance dynamicByteLengthFloat32BE :: DynamicByteLength Float32BE where
  byteLength _ = pure 4
instance dynamicByteLengthFloat32LE :: DynamicByteLength Float32LE where
  byteLength _ = pure 4
instance dynamicByteLengthFloat64BE :: DynamicByteLength Float64BE where
  byteLength _ = pure 8
instance dynamicByteLengthFloat64LE :: DynamicByteLength Float64LE where
  byteLength _ = pure 8
instance dynamicByteLengthUnit :: DynamicByteLength Unit where
  byteLength _ = pure 0
instance dynamicByteLengthVoid :: DynamicByteLength Void where
  byteLength _ = pure 0
instance dynamicByteLengthBoolean :: DynamicByteLength Boolean where
  byteLength _ = pure 1
instance dynamicByteLengthOrdering :: DynamicByteLength Ordering where
  byteLength _ = pure 1
instance dynamicByteLengthCodePoint :: DynamicByteLength CodePoint where
  byteLength c = case fromEnum c of
    v | v <= 0x7f -> pure 1
      | v <= 0x7ff -> pure 2
      | v <= 0xffff -> pure 3
      | v <= 0x10ffff -> pure 4
      | otherwise -> throw ("CodePoint not in unicode range: " <> show c)
instance dynamicByteLengthChar :: DynamicByteLength Char where
  byteLength c = byteLength (codePointFromChar c)
instance dynamicByteLengthString :: DynamicByteLength String where
  byteLength xs = byteLength (toCodePointArray xs)
instance dynamicByteLengthMaybe :: DynamicByteLength a => DynamicByteLength (Maybe a) where
  byteLength mX = case mX of
    Nothing -> pure 1
    Just x -> (_ + 1) <$> byteLength x
instance dynamicByteLengthTuple :: (DynamicByteLength a, DynamicByteLength b) => DynamicByteLength (Tuple a b) where
  byteLength (Tuple x y) = (+) <$> byteLength x <*> byteLength y
instance dynamicByteLengthEither :: (DynamicByteLength a, DynamicByteLength b) => DynamicByteLength (Either a b) where
  byteLength eXY = (_ + 1) <$> case eXY of
    Left x -> byteLength x
    Right y -> byteLength y
instance dynamicByteLengthArray :: DynamicByteLength a => DynamicByteLength (Array a) where
  byteLength xs = (\ys -> sum ys + 4) <$> traverse byteLength xs
instance dynamicByteLengthList :: DynamicByteLength a => DynamicByteLength (List a) where
  byteLength xs = byteLength (Array.fromFoldable xs)
instance dynamicByteLengthNonEmptyArray :: DynamicByteLength a => DynamicByteLength (NonEmpty Array a) where
  byteLength (NonEmpty x xs) = byteLength (Array.cons x xs)
instance dynamicByteLengthNonEmptyList :: DynamicByteLength a => DynamicByteLength (NonEmpty List a) where
  byteLength (NonEmpty x xs) = byteLength (Cons x xs)
instance dynamicByteLengthAdditive :: DynamicByteLength a => DynamicByteLength (Additive a) where
  byteLength (Additive x) = byteLength x
instance dynamicByteLengthMultiplicative :: DynamicByteLength a => DynamicByteLength (Multiplicative a) where
  byteLength (Multiplicative x) = byteLength x
instance dynamicByteLengthEndo :: DynamicByteLength (c a a) => DynamicByteLength (Endo c a) where
  byteLength (Endo x) = byteLength x
instance dynamicByteLengthDual :: DynamicByteLength a => DynamicByteLength (Dual a) where
  byteLength (Dual x) = byteLength x
instance dynamicByteLengthDisj :: DynamicByteLength a => DynamicByteLength (Disj a) where
  byteLength (Disj x) = byteLength x
instance dynamicByteLengthConj :: DynamicByteLength a => DynamicByteLength (Conj a) where
  byteLength (Conj x) = byteLength x
instance dynamicByteLengthObject :: DynamicByteLength a => DynamicByteLength (O.Object a) where
  byteLength xs = byteLength (O.toAscUnfoldable xs :: Array (Tuple String a))
instance dynamicByteLengthSet :: DynamicByteLength a => DynamicByteLength (Set.Set a) where
  byteLength xs = byteLength (Set.toUnfoldable xs :: Array a)
instance dynamicByteLengthMap :: (DynamicByteLength a, DynamicByteLength k) => DynamicByteLength (Map.Map k a) where
  byteLength xs = byteLength (Map.toUnfoldable xs :: Array (Tuple k a))
instance dynamicByteLengthHashSet :: DynamicByteLength a => DynamicByteLength (HS.HashSet a) where
  byteLength xs = byteLength (HS.toArray xs)
instance dynamicByteLengthHashMap :: (DynamicByteLength k, DynamicByteLength a) => DynamicByteLength (HM.HashMap k a) where
  byteLength xs = byteLength (HM.toArrayBy Tuple xs)
instance dynamicByteLengthArrayBuffer :: DynamicByteLength ArrayBuffer where
  byteLength xs = pure (AB.byteLength xs + 4)
instance dynamicByteLengthDataView :: DynamicByteLength DataView where
  byteLength xs = byteLength (DV.buffer xs)
instance dynamicByteLengthArrayView :: DynamicByteLength (ArrayView a) where
  byteLength xs = byteLength (TA.buffer xs)
instance dynamicByteLengthAV :: DynamicByteLength (AV a t) where
  byteLength (AV xs) = byteLength xs
instance dynamicByteLengthRecord :: GEncodeArrayBuffer row list => DynamicByteLength (Record row) where
  byteLength xs = gPutArrayBuffer xs (RLProxy :: RLProxy list) >>= byteLength



class EncodeArrayBuffer a where
  -- | Returns the bytes written indicating _partial_ success - for exact success, the written
  -- | bytes should equal the value's dynamic `byteLength`. This function should
  -- | strictly write to the `ArrayBuffer`, and shouldn't attempt to read it.
  putArrayBuffer :: ArrayBuffer -- ^ Storage medium
                 -> ByteOffset -- ^ Position to store
                 -> a -- ^ Value to store
                 -> Effect (Maybe ByteLength)

class DecodeArrayBuffer a where
  -- | Returns the value parsed if successful.
  -- | This function should strictly read the `ArrayBuffer`, and shouldn't attempt to write to it.
  readArrayBuffer :: ArrayBuffer -- ^ Storage medium
                  -> ByteOffset -- ^ Position to read from
                  -> Effect (Maybe a)

-- Numeric instances

instance encodeArrayBufferUint32BE :: EncodeArrayBuffer Uint32BE where
  putArrayBuffer b o a@(Uint32BE x) = DV.setUint32be (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferUint32LE :: EncodeArrayBuffer Uint32LE where
  putArrayBuffer b o a@(Uint32LE x) = DV.setUint32le (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferUint16BE :: EncodeArrayBuffer Uint16BE where
  putArrayBuffer b o a@(Uint16BE x) = DV.setUint16be (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferUint16LE :: EncodeArrayBuffer Uint16LE where
  putArrayBuffer b o a@(Uint16LE x) = DV.setUint16le (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferUint8 :: EncodeArrayBuffer Uint8 where
  putArrayBuffer b o a@(Uint8 x) = DV.setUint8 (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferInt32BE :: EncodeArrayBuffer Int32BE where
  putArrayBuffer b o a@(Int32BE x) = DV.setInt32be (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferInt32LE :: EncodeArrayBuffer Int32LE where
  putArrayBuffer b o a@(Int32LE x) = DV.setInt32le (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferInt16BE :: EncodeArrayBuffer Int16BE where
  putArrayBuffer b o a@(Int16BE x) = DV.setInt16be (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferInt16LE :: EncodeArrayBuffer Int16LE where
  putArrayBuffer b o a@(Int16LE x) = DV.setInt16le (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferInt8 :: EncodeArrayBuffer Int8 where
  putArrayBuffer b o a@(Int8 x) = DV.setInt8 (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferFloat32BE :: EncodeArrayBuffer Float32BE where
  putArrayBuffer b o a@(Float32BE x) = DV.setFloat32be (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferFloat32LE :: EncodeArrayBuffer Float32LE where
  putArrayBuffer b o a@(Float32LE x) = DV.setFloat32le (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferFloat64BE :: EncodeArrayBuffer Float64BE where
  putArrayBuffer b o a@(Float64BE x) = DV.setFloat64be (DV.whole b) o x >>= guardByteLength a
instance encodeArrayBufferFloat64LE :: EncodeArrayBuffer Float64LE where
  putArrayBuffer b o a@(Float64LE x) = DV.setFloat64le (DV.whole b) o x >>= guardByteLength a

guardByteLength :: forall a. DynamicByteLength a => a -> Boolean -> Effect (Maybe ByteLength)
guardByteLength x success =
  if success
    then Just <$> byteLength x
    else pure Nothing


instance decodeArrayBufferUint32BE :: DecodeArrayBuffer Uint32BE where
  readArrayBuffer b o = (map Uint32BE) <$> DV.getUint32be (DV.whole b) o
instance decodeArrayBufferUint32LE :: DecodeArrayBuffer Uint32LE where
  readArrayBuffer b o = (map Uint32LE) <$> DV.getUint32le (DV.whole b) o
instance decodeArrayBufferUint16BE :: DecodeArrayBuffer Uint16BE where
  readArrayBuffer b o = (map Uint16BE) <$> DV.getUint16be (DV.whole b) o
instance decodeArrayBufferUint16LE :: DecodeArrayBuffer Uint16LE where
  readArrayBuffer b o = (map Uint16LE) <$> DV.getUint16le (DV.whole b) o
instance decodeArrayBufferUint8 :: DecodeArrayBuffer Uint8 where
  readArrayBuffer b o = (map Uint8) <$> DV.getUint8 (DV.whole b) o
instance decodeArrayBufferInt32BE :: DecodeArrayBuffer Int32BE where
  readArrayBuffer b o = (map Int32BE) <$> DV.getInt32be (DV.whole b) o
instance decodeArrayBufferInt32LE :: DecodeArrayBuffer Int32LE where
  readArrayBuffer b o = (map Int32LE) <$> DV.getInt32le (DV.whole b) o
instance decodeArrayBufferInt16BE :: DecodeArrayBuffer Int16BE where
  readArrayBuffer b o = (map Int16BE) <$> DV.getInt16be (DV.whole b) o
instance decodeArrayBufferInt16LE :: DecodeArrayBuffer Int16LE where
  readArrayBuffer b o = (map Int16LE) <$> DV.getInt16le (DV.whole b) o
instance decodeArrayBufferInt8 :: DecodeArrayBuffer Int8 where
  readArrayBuffer b o = (map Int8) <$> DV.getInt8 (DV.whole b) o
instance decodeArrayBufferFloat32BE :: DecodeArrayBuffer Float32BE where
  readArrayBuffer b o = (map Float32BE) <$> DV.getFloat32be (DV.whole b) o
instance decodeArrayBufferFloat32LE :: DecodeArrayBuffer Float32LE where
  readArrayBuffer b o = (map Float32LE) <$> DV.getFloat32le (DV.whole b) o
instance decodeArrayBufferFloat64BE :: DecodeArrayBuffer Float64BE where
  readArrayBuffer b o = (map Float64BE) <$> DV.getFloat64be (DV.whole b) o
instance decodeArrayBufferFloat64LE :: DecodeArrayBuffer Float64LE where
  readArrayBuffer b o = (map Float64LE) <$> DV.getFloat64le (DV.whole b) o



-- * Casual instances

instance encodeArrayBufferUnit :: EncodeArrayBuffer Unit where
  putArrayBuffer b o _ = pure (Just 0)
instance decodeArrayBufferUnit :: DecodeArrayBuffer Unit where
  readArrayBuffer b o = pure (Just unit)
instance encodeArrayBufferVoid :: EncodeArrayBuffer Void where
  putArrayBuffer b o _ = pure (Just 0)
instance decodeArrayBufferVoid :: DecodeArrayBuffer Void where
  readArrayBuffer b o = throw "Cannot decode a Void value"
-- | Encodes the boolean into an unsigned word8
instance encodeArrayBufferBoolean :: EncodeArrayBuffer Boolean where
  putArrayBuffer b o x =
    let v = if x then 1 else 0
    in  putArrayBuffer b o (Uint8 (UInt.fromInt v))
instance decodeArrayBufferBoolean :: DecodeArrayBuffer Boolean where
  readArrayBuffer b o =
    let f mV = case mV of
          Nothing -> pure Nothing
          Just (Uint8 v) ->
            let v' = UInt.toInt v
            in  if v' == 0 then pure (Just false)
                  else if v' == 1 then pure (Just true)
                    else throw ("Not a Boolean ArrayBuffer encoding: " <> show v)
    in  readArrayBuffer b o >>= f
instance encodeArrayBufferOrdering :: EncodeArrayBuffer Ordering where
  putArrayBuffer b o x =
    let v = case x of
              LT -> 0
              EQ -> 1
              GT -> 2
    in  putArrayBuffer b o (Uint8 (UInt.fromInt v))
instance decodeArrayBufferOrdering :: DecodeArrayBuffer Ordering where
  readArrayBuffer b o =
    let f mV = case mV of
          Nothing -> pure Nothing
          Just (Uint8 v) ->
            let v' = UInt.toInt v
            in  case unit of
                  _ | v' == 0 -> pure (Just LT)
                    | v' == 1 -> pure (Just EQ)
                    | v' == 2 -> pure (Just GT)
                    | otherwise -> throw ("Not a Boolean ArrayBuffer encoding: " <> show v)
    in  readArrayBuffer b o >>= f
instance encodeArrayBufferCodePoint :: EncodeArrayBuffer CodePoint where
  putArrayBuffer b o c =
    let v = fromEnum c
        z = v .&. 0x3f
        y = (shr v 6) .&. 0x3f
        x = (shr v 12) .&. 0x3f
        w = (shr v 18) .&. 0x7
        mkVal = Uint8 <<< UInt.fromInt
    in  case unit of
          _ | v <= 0x7f -> putArrayBuffer b o (Uint8 (UInt.fromInt v))
            | v <= 0x7ff -> do
              mW <- putArrayBuffer b o (mkVal (0xc0 .|. y))
              case mW of
                Nothing -> pure Nothing
                Just written -> do
                  mW' <- putArrayBuffer b (o + written) (mkVal (0x80 .|. z))
                  case mW' of
                    Nothing -> pure (Just written)
                    Just written' -> pure (Just (written + written'))
            | v <= 0xffff -> do
              mW <- putArrayBuffer b o (mkVal (0xe0 .|. x))
              case mW of
                Nothing -> pure Nothing
                Just written -> do
                  mW' <- putArrayBuffer b (o + written) (mkVal (0x80 .|. y))
                  case mW' of
                    Nothing -> pure (Just written)
                    Just written' -> do
                      mW'' <- putArrayBuffer b (o + written + written') (mkVal (0x80 .|. z))
                      case mW'' of
                        Nothing -> pure (Just (written + written'))
                        Just written'' -> pure (Just (written + written' + written''))
            | v <= 0x10ffff -> do
              mW <- putArrayBuffer b o (mkVal (0xf0 .|. w))
              case mW of
                Nothing -> pure Nothing
                Just written -> do
                  let o' = o + written
                  mW' <- putArrayBuffer b o' (mkVal (0x80 .|. x))
                  case mW' of
                    Nothing -> pure (Just written)
                    Just written' -> do
                      let o'' = o' + written'
                      mW'' <- putArrayBuffer b o'' (mkVal (0x80 .|. y))
                      case mW'' of
                        Nothing -> pure (Just (written + written'))
                        Just written'' -> do
                          let o''' = o'' + written''
                          mW''' <- putArrayBuffer b o''' (mkVal (0x80 .|. z))
                          case mW''' of
                            Nothing -> pure (Just (written + written' + written''))
                            Just written''' ->
                              pure (Just (written + written' + written'' + written'''))
            | otherwise -> throw ("Char not in unicode range: " <> show c)
instance decodeArrayBufferCodePoint :: DecodeArrayBuffer CodePoint where
  readArrayBuffer b o = do
    let readNextByte o' = (map (\(Uint8 v) -> UInt.toInt v)) <$> readArrayBuffer b o'
        shl6 x = shl x 6
    mW <- readNextByte o
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        r <-
          if w < 0x80 then pure w
            else do
              mX <- (map (xor 0x80)) <$> readNextByte (o + 1)
              case mX of
                Nothing -> throw ("Incorrect unicode char encoding - w: " <> show w)
                Just x ->
                  if w < 0xe0 then pure (x .|. shl6 (xor 0xc0 w))
                    else do
                      mY <- (map (xor 0x80)) <$> readNextByte (o + 2)
                      case mY of
                        Nothing -> throw ("Incorrect unicode char encoding - w: " <> show w <> ", x: " <> show x)
                        Just y ->
                          if w < 0xf0 then pure (y .|. shl6 (x .|. shl6 (xor 0xe0 w)))
                            else do
                              mZ <- (map (xor 0x80)) <$> readNextByte (o + 3)
                              case mZ of
                                Nothing -> throw ("Incorrect unicode char encoding - w: " <> show w <> ", x: " <> show x <> ", y: " <> show y)
                                Just z -> pure (z .|. shl6 (y .|. shl6 (x .|. shl6 (xor 0xf0 w))))
        case toEnum r of -- FIXME sure this works for Ints, and shouldn't be UInts? It's up to 4 bytes...
          Nothing -> throw ("Incorrect unicode char encoding: " <> show r)
          Just c -> pure (Just c)
instance encodeArrayBufferChar :: EncodeArrayBuffer Char where
  putArrayBuffer b o x = putArrayBuffer b o (codePointFromChar x)
instance decodeArrayBufferChar :: DecodeArrayBuffer Char where
  readArrayBuffer b o =
    let codePointToChar mc = case mc of
          Nothing -> pure Nothing
          Just c -> case toChar (singleton c) of
            Nothing -> throw ("Code Point not capable of being a char: " <> show c)
            Just c' -> pure (Just c')
    in  readArrayBuffer b o >>= codePointToChar

-- Strings FIXME is exactly utf8 encoded? Composite types would have to include their byte length
-- for every sub-field if that were the case...

instance encodeArrayBufferString :: EncodeArrayBuffer String where
  putArrayBuffer b o xs = putArrayBuffer b o (toCodePointArray xs)
instance decodeArrayBufferString :: DecodeArrayBuffer String where
  readArrayBuffer b o = (map fromCodePointArray) <$> readArrayBuffer b o



-- Monoid newtypes

instance encodeArrayBufferAdditive :: EncodeArrayBuffer a => EncodeArrayBuffer (Additive a) where
  putArrayBuffer b o (Additive x) = putArrayBuffer b o x
instance decodeArrayBufferAdditive :: DecodeArrayBuffer a => DecodeArrayBuffer (Additive a) where
  readArrayBuffer b o = (map Additive) <$> readArrayBuffer b o
instance encodeArrayBufferMultiplicative :: EncodeArrayBuffer a => EncodeArrayBuffer (Multiplicative a) where
  putArrayBuffer b o (Multiplicative x) = putArrayBuffer b o x
instance decodeArrayBufferMultiplicative :: DecodeArrayBuffer a => DecodeArrayBuffer (Multiplicative a) where
  readArrayBuffer b o = (map Multiplicative) <$> readArrayBuffer b o
instance encodeArrayBufferConj :: EncodeArrayBuffer a => EncodeArrayBuffer (Conj a) where
  putArrayBuffer b o (Conj x) = putArrayBuffer b o x
instance decodeArrayBufferConj :: DecodeArrayBuffer a => DecodeArrayBuffer (Conj a) where
  readArrayBuffer b o = (map Conj) <$> readArrayBuffer b o
instance encodeArrayBufferDisj :: EncodeArrayBuffer a => EncodeArrayBuffer (Disj a) where
  putArrayBuffer b o (Disj x) = putArrayBuffer b o x
instance decodeArrayBufferDisj :: DecodeArrayBuffer a => DecodeArrayBuffer (Disj a) where
  readArrayBuffer b o = (map Disj) <$> readArrayBuffer b o
instance encodeArrayBufferDual :: EncodeArrayBuffer a => EncodeArrayBuffer (Dual a) where
  putArrayBuffer b o (Dual x) = putArrayBuffer b o x
instance decodeArrayBufferDual :: DecodeArrayBuffer a => DecodeArrayBuffer (Dual a) where
  readArrayBuffer b o = (map Dual) <$> readArrayBuffer b o
instance encodeArrayBufferEndo :: EncodeArrayBuffer (c a a) => EncodeArrayBuffer (Endo c a) where
  putArrayBuffer b o (Endo x) = putArrayBuffer b o x
instance decodeArrayBufferEndo :: DecodeArrayBuffer (c a a) => DecodeArrayBuffer (Endo c a) where
  readArrayBuffer b o = (map Endo) <$> readArrayBuffer b o



-- Trivial containers

instance encodeArrayBufferMaybe :: EncodeArrayBuffer a => EncodeArrayBuffer (Maybe a) where
  putArrayBuffer b o mX = case mX of
    Nothing -> putArrayBuffer b o (Uint8 (UInt.fromInt 0))
    Just x -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 1))
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBuffer b (o + w) x
          case mW' of
            Nothing -> pure (Just w) -- FIXME warn?
            Just w' -> pure (Just (w' + w))
instance decodeArrayBufferMaybe :: DecodeArrayBuffer a => DecodeArrayBuffer (Maybe a) where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just (Uint8 i) ->
        let i' = UInt.toInt i
        in  case unit of
              _ | i' == 0 -> pure (Just Nothing)
                | i' == 1 -> do
                mX' <- readArrayBuffer b (o + 1)
                case mX' of
                  Nothing -> throw "Incorrect Maybe encoding - got Just flag, but no data"
                  Just x -> pure (Just (Just x))
                | otherwise -> throw ("Incorrect Maybe encoding - flag out of range: " <> show i')
instance encodeArrayBufferTuple :: (EncodeArrayBuffer a, EncodeArrayBuffer b) => EncodeArrayBuffer (Tuple a b) where
  putArrayBuffer b o (Tuple x y) = do
    mW <- putArrayBuffer b o x
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        mW' <- putArrayBuffer b (o + w) y
        case mW' of
          Nothing -> pure (Just w) -- FIXME warn?
          Just w' -> pure (Just (w + w'))
instance decodeArrayBufferTuple :: (DecodeArrayBuffer a, DecodeArrayBuffer b, DynamicByteLength a, DynamicByteLength b) => DecodeArrayBuffer (Tuple a b) where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just x -> do
        l <- byteLength x
        mY <- readArrayBuffer b (o + l)
        case mY of
          Nothing -> throw "Incorrect Tuple encoding - got one value, but not the next"
          Just y -> pure (Just (Tuple x y))
instance encodeArrayBufferEither :: (EncodeArrayBuffer a, EncodeArrayBuffer b) => EncodeArrayBuffer (Either a b) where
  putArrayBuffer b o eXY = case eXY of
    Left x -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 0))
      case mW of
        Nothing -> pure Nothing
        Just _ -> do
          mW' <- putArrayBuffer b (o + 1) x
          case mW' of
            Nothing -> pure (Just 1) -- FIXME warn?
            Just w' -> pure (Just (w' + 1))
    Right y -> do
      mW <- putArrayBuffer b o (Uint8 (UInt.fromInt 1))
      case mW of
        Nothing -> pure Nothing
        Just _ -> do
          mW' <- putArrayBuffer b (o + 1) y
          case mW' of
            Nothing -> pure (Just 1) -- FIXME warn?
            Just w' -> pure (Just (w' + 1))
instance decodeArrayBufferEither :: (DecodeArrayBuffer a, DecodeArrayBuffer b, DynamicByteLength a, DynamicByteLength b) => DecodeArrayBuffer (Either a b) where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just (Uint8 i) ->
        let i' = UInt.toInt i
        in  case unit of
              _ | i' == 0 -> do
                mX' <- readArrayBuffer b (o + 1)
                case mX' of
                  Nothing -> throw "Incorrect Either encoding - got Left flag, but no data"
                  Just x -> pure (Just (Left x))
                | i' == 1 -> do
                mY' <- readArrayBuffer b (o + 1)
                case mY' of
                  Nothing -> throw "Incorrect Either encoding - got Right flag, but no data"
                  Just y -> pure (Just (Right y))
                | otherwise -> throw ("Incorrect Either encoding - flag out of range: " <> show i')
instance encodeArrayBufferArray :: EncodeArrayBuffer a => EncodeArrayBuffer (Array a) where
  putArrayBuffer b o xs = do
    let l :: Number
        l = length xs
    mW <- putArrayBuffer b o (Uint32BE (UInt.fromNumber l)) -- put array length as unsigned 32
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        nextORef <- Ref.new (o + w) -- w should be 4
        for_ xs \x -> do
          o' <- Ref.read nextORef
          mW' <- putArrayBuffer b o' x -- put each (possibly variadic) entity
          case mW' of
            Nothing -> throw ("Incorrect ArrayBuffer length - wrote array length and possible bytes: " <> show o')
            Just w' -> Ref.write (o' + w') nextORef
        withOffset <- Ref.read nextORef
        pure (Just (withOffset - o))
instance decodeArrayBufferArray :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (Array a) where
  readArrayBuffer b o = do
    mL <- readArrayBuffer b o
    case mL of
      Nothing -> pure Nothing
      Just (Uint32BE l) -> do
        nextORef <- Ref.new (o + 4)
        Just <$> replicateA (unsafeCoerce (UInt.toNumber l)) do
          o' <- Ref.read nextORef
          mX <- readArrayBuffer b o'
          case mX of
            Nothing -> throw ("Incorrect ArrayBuffer encoding - retreived length, but not all values: " <> show o')
            Just x -> do
              l' <- byteLength x
              Ref.write (o' + l') nextORef
              pure x
instance encodeArrayBufferList :: EncodeArrayBuffer a => EncodeArrayBuffer (List a) where
  putArrayBuffer b o xs = putArrayBuffer b o (Array.fromFoldable xs)
instance decodeArrayBufferList :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (List a) where
  readArrayBuffer b o = (map Array.toUnfoldable) <$> readArrayBuffer b o
instance encodeArrayBufferNonEmptyArray :: EncodeArrayBuffer a => EncodeArrayBuffer (NonEmpty Array a) where
  putArrayBuffer b o (NonEmpty x xs) = putArrayBuffer b o (Array.cons x xs)
instance decodeArrayBufferNonEmptyArray :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (NonEmpty Array a) where
  readArrayBuffer b o = do
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> case Array.uncons xs of
        Nothing -> throw ("Incorrect NonEmpty Array encoding - array is empty")
        Just {head,tail} -> pure (Just (NonEmpty head tail))
instance encodeArrayBufferNonEmptyList :: EncodeArrayBuffer a => EncodeArrayBuffer (NonEmpty List a) where
  putArrayBuffer b o (NonEmpty x xs) = putArrayBuffer b o (Cons x xs)
instance decodeArrayBufferNonEmptyList :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (NonEmpty List a) where
  readArrayBuffer b o = do
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> case xs of
        Nil -> throw ("Incorrect NonEmpty List encoding - list is empty")
        Cons head tail -> pure (Just (NonEmpty head tail))

-- Larger containers

instance encodeArrayBufferObject :: EncodeArrayBuffer a => EncodeArrayBuffer (O.Object a) where
  putArrayBuffer b o xs = putArrayBuffer b o (O.toAscUnfoldable xs :: Array (Tuple String a))
instance decodeArrayBufferObject :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (O.Object a) where
  readArrayBuffer b o = (map (O.fromFoldable :: Array (Tuple String a) -> O.Object a)) <$> readArrayBuffer b o
instance encodeArrayBufferSet :: EncodeArrayBuffer a => EncodeArrayBuffer (Set.Set a) where
  putArrayBuffer b o xs = putArrayBuffer b o (Set.toUnfoldable xs :: Array a)
instance decodeArrayBufferSet :: (Ord a, DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (Set.Set a) where
  readArrayBuffer b o = (map (Set.fromFoldable :: Array a -> Set.Set a)) <$> readArrayBuffer b o
instance encodeArrayBufferMap :: (EncodeArrayBuffer a, EncodeArrayBuffer k) => EncodeArrayBuffer (Map.Map k a) where
  putArrayBuffer b o xs = putArrayBuffer b o (Map.toUnfoldable xs :: Array (Tuple k a))
instance decodeArrayBufferMap :: (Ord k, DecodeArrayBuffer k, DecodeArrayBuffer a, DynamicByteLength k, DynamicByteLength a) => DecodeArrayBuffer (Map.Map k a) where
  readArrayBuffer b o = (map (Map.fromFoldable :: Array (Tuple k a) -> Map.Map k a)) <$> readArrayBuffer b o
instance encodeArrayBufferHashSet :: EncodeArrayBuffer a => EncodeArrayBuffer (HS.HashSet a) where
  putArrayBuffer b o xs = putArrayBuffer b o (HS.toArray xs)
instance decodeArrayBufferHashSet :: (Hashable a, DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (HS.HashSet a) where
  readArrayBuffer b o = (map (HS.fromFoldable :: Array a -> HS.HashSet a)) <$> readArrayBuffer b o
instance encodeArrayBufferHashMap :: (EncodeArrayBuffer k, EncodeArrayBuffer a) => EncodeArrayBuffer (HM.HashMap k a) where
  putArrayBuffer b o xs = putArrayBuffer b o (HM.toArrayBy Tuple xs)
instance decodeArrayBufferHashMap :: (Hashable k, DecodeArrayBuffer k, DecodeArrayBuffer a, DynamicByteLength k, DynamicByteLength a) => DecodeArrayBuffer (HM.HashMap k a) where
  readArrayBuffer b o = (map HM.fromArray) <$> readArrayBuffer b o


-- Records

instance encodeArrayBufferRecord ::
  ( GEncodeArrayBuffer row list
  , RL.RowToList row list
  ) => EncodeArrayBuffer (Record row) where
  putArrayBuffer b o xs = gPutArrayBuffer xs (RLProxy :: RLProxy list) >>= putArrayBuffer b o

instance decodeArrayBufferRecord ::
  ( GDecodeArrayBuffer row list
  , RL.RowToList row list
  ) => DecodeArrayBuffer (Record row) where
  readArrayBuffer b o = do
    mObject <- readArrayBuffer b o
    case mObject of
      Nothing -> pure Nothing
      Just object -> Just <$> gReadArrayBuffer object (RLProxy :: RLProxy list)



-- ArrayBuffers

instance encodeArrayBufferArrayBuffer :: EncodeArrayBuffer ArrayBuffer where
  putArrayBuffer b o xs = do
    l <- byteLength xs
    mW <- putArrayBuffer b o (Uint32BE (UInt.fromNumber (unsafeCoerce l)))
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        (ta :: Uint8Array) <- TA.whole xs
        let target = DV.whole b
        offsetRef <- Ref.new (o + w)
        let go x = do
              o' <- Ref.modify (_ + 1) offsetRef
              s <- DV.setUint8 target o' x
              if s then pure unit else throw ("Not enough room for the ArrayBuffer in the target: " <> show o')
        TA.traverse_ go ta
        Just <$> Ref.read offsetRef
instance decodeArrayBufferArrayBuffer :: DecodeArrayBuffer ArrayBuffer where
  readArrayBuffer b o = do
    mL <- readArrayBuffer b o
    case mL of
      Nothing -> pure Nothing
      Just (Uint32BE l) ->
        let l' = unsafeCoerce (UInt.toNumber l)
        in  pure (Just (AB.slice (o + 4) (o + 4 + l') b))
instance encodeArrayBufferDataView :: EncodeArrayBuffer DataView where
  putArrayBuffer b o xs = putArrayBuffer b o (DV.buffer xs)
instance decodeArrayBufferDataView :: DecodeArrayBuffer DataView where
  readArrayBuffer b o = (map DV.whole) <$> readArrayBuffer b o
instance encodeArrayBufferArrayView :: EncodeArrayBuffer (ArrayView a) where
  putArrayBuffer b o xs = putArrayBuffer b o (TA.buffer xs)
instance decodeArrayBufferArrayView :: TA.TypedArray a t => DecodeArrayBuffer (ArrayView a) where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just x -> Just <$> TA.whole x
instance encodeArrayBufferAV :: EncodeArrayBuffer (AV a t) where
  putArrayBuffer b o (AV xs) = putArrayBuffer b o xs
instance decodeArrayBufferAV :: TA.TypedArray a t => DecodeArrayBuffer (AV a t) where
  readArrayBuffer b o = (map AV) <$> readArrayBuffer b o



-- TODO RowToList for Rows
-- TODO generics
-- TODO Vec? Sized in advance?


-- | Generate a new `ArrayBuffer` from a value. Throws an `Error` if writing fails, or if the written bytes
-- | do not equal the dynamic `byteLength` of the value.
encodeArrayBuffer :: forall a. EncodeArrayBuffer a => DynamicByteLength a => a -> Effect ArrayBuffer
encodeArrayBuffer x = do
  l <- byteLength x
  b <- AB.empty l
  mW <- putArrayBuffer b 0 x
  case mW of
    Nothing -> throw "Couldn't serialize to ArrayBuffer"
    Just w
      | w /= l -> throw ("Written bytes and dynamic byte length are not identical - written: " <> show w <> ", byte length: " <> show l)
      | otherwise -> pure b


-- | Attempt to parse a value from an `ArrayBuffer`, starting at the first index.
decodeArrayBuffer :: forall a. DecodeArrayBuffer a => ArrayBuffer -> Effect (Maybe a)
decodeArrayBuffer b = readArrayBuffer b 0


class GEncodeArrayBuffer (row :: # Type) (list :: RL.RowList) where
  gPutArrayBuffer :: Record row -> RLProxy list -> Effect (O.Object ArrayBuffer)

instance gEncodeArrayBufferNil :: GEncodeArrayBuffer row RL.Nil where
  gPutArrayBuffer _ _ = pure O.empty

instance gEncodeArrayBufferCons ::
  ( EncodeArrayBuffer value
  , DynamicByteLength value
  , GEncodeArrayBuffer row tail
  , IsSymbol field
  , Cons field value tail' row
  ) => GEncodeArrayBuffer row (RL.Cons field value tail) where
  gPutArrayBuffer row _ = do
    let sProxy :: SProxy field
        sProxy = SProxy
        value :: value
        value = Record.get sProxy row
    x <- encodeArrayBuffer value
    rest <- gPutArrayBuffer row (RLProxy :: RLProxy tail)
    pure (O.insert (reflectSymbol sProxy) x rest)


class GDecodeArrayBuffer (row :: # Type) (list :: RL.RowList) | list -> row where
  gReadArrayBuffer :: O.Object ArrayBuffer -> RLProxy list -> Effect (Record row)

instance gDecodeArrayBufferNil :: GDecodeArrayBuffer () RL.Nil where
  gReadArrayBuffer _ _ = pure {}

instance gDecodeArrayBufferCons ::
  ( DecodeArrayBuffer value
  , GDecodeArrayBuffer rowTail tail
  , IsSymbol field
  , Cons field value rowTail row
  , Lacks field rowTail
  ) => GDecodeArrayBuffer row (RL.Cons field value tail) where
  gReadArrayBuffer object _ = do
    let sProxy :: SProxy field
        sProxy = SProxy
        fieldName = reflectSymbol sProxy
    rest <- gReadArrayBuffer object (RLProxy :: RLProxy tail)
    case O.lookup fieldName object of
      Nothing -> throw ("Record was missing expected field: " <> fieldName)
      Just b -> do
        mVal <- decodeArrayBuffer b
        case mVal of
          Nothing -> throw ("Couldn't decode arraybuffer stored in object: " <> fieldName)
          Just val -> pure (Record.insert sProxy val rest)
