module Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, putArrayBuffer
  , class DecodeArrayBuffer, readArrayBuffer
  , class DynamicByteLength, byteLength
  , encodeArrayBuffer
  , decodeArrayBuffer
  , module Data.ArrayBuffer.Class.Types
  ) where

import Data.ArrayBuffer.Class.Types
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.ArrayBuffer.DataView as DV

import Prelude
  ( Unit, Ordering (..)
  , (<$>), (>>=), (<<<), (<=), (<>), (+), (/=), (==), (<)
  , pure, otherwise, show, unit, bind, map)
import Data.Maybe (Maybe (..))
import Data.UInt (fromInt, toInt) as UInt
import Data.Char (toCharCode, fromCharCode)
import Data.Int.Bits ((.|.), (.&.), shr, shl, xor)
import Effect (Effect)
import Effect.Exception (throw)


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
instance dynamicByteLengthBoolean :: DynamicByteLength Boolean where
  byteLength _ = pure 1
instance dynamicByteLengthOrdering :: DynamicByteLength Ordering where
  byteLength _ = pure 1
instance dynamicByteLengthChar :: DynamicByteLength Char where
  byteLength c = case toCharCode c of
    v | v <= 0x7f -> pure 1
      | v <= 0x7ff -> pure 2
      | v <= 0xffff -> pure 3
      | v <= 0x10ffff -> pure 4
      | otherwise -> throw ("Char not in unicode range: " <> show c)


class DynamicByteLength a <= EncodeArrayBuffer a where
  -- | Returns the bytes written indicating _partial_ success - for exact success, the written
  -- | bytes should equal the value's dynamic `byteLength`. This function should
  -- | strictly write to the `ArrayBuffer`, and shouldn't attempt to read it.
  putArrayBuffer :: ArrayBuffer -- ^ Storage medium
                 -> ByteOffset -- ^ Position to store
                 -> a -- ^ Value to store
                 -> Effect (Maybe ByteLength)

class DynamicByteLength a <= DecodeArrayBuffer a where
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
instance encodeArrayBufferChar :: EncodeArrayBuffer Char where
  putArrayBuffer b o c =
    let v = toCharCode c
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
instance decodeArrayBufferChar :: DecodeArrayBuffer Char where
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
        case fromCharCode r of -- FIXME sure this works for Ints, and shouldn't be UInts? It's up to 4 bytes...
          Nothing -> throw ("Incorrect unicode char encoding: " <> show r)
          Just c -> pure (Just c)



-- | Generate a new `ArrayBuffer` from a value. Throws an `Error` if writing fails, or if the written bytes
-- | do not equal the dynamic `byteLength` of the value.
encodeArrayBuffer :: forall a. EncodeArrayBuffer a => a -> Effect ArrayBuffer
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
