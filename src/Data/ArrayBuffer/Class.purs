module Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, putArrayBuffer
  , class DecodeArrayBuffer, readArrayBuffer
  , class DynamicByteLength, byteLength
  , module Data.ArrayBuffer.Class.Types
  ) where

import Data.ArrayBuffer.Class.Types
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.ArrayBuffer.DataView as DV

import Prelude ((<$>), (>>=), pure)
import Data.Maybe (Maybe (..))
import Effect (Effect)


class DynamicByteLength a where
  -- | Get the byte length of a value at runtime.
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


class DynamicByteLength a <= EncodeArrayBuffer a where
  -- | Returns the byte length of the stored value, if successful. This function should
  -- | strictly write to the `ArrayBuffer`, and shouldn't attempt to read it.
  putArrayBuffer :: ArrayBuffer -- ^ Storage medium
                 -> ByteOffset -- ^ Position to store
                 -> a -- ^ Value to store
                 -> Effect (Maybe {written :: ByteLength})

class DynamicByteLength a <= DecodeArrayBuffer a where
  -- | Returns the value parsed, and the byte length of the parsed string, if successful.
  -- | This function should strictly read the `ArrayBuffer`, and shouldn't attempt to write to it.
  readArrayBuffer :: ArrayBuffer -- ^ Storage medium
                  -> ByteOffset -- ^ Position to read from
                  -> Effect (Maybe {value :: a, consumed :: ByteLength})


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

guardByteLength :: forall a. DynamicByteLength a => a -> Boolean -> Effect (Maybe {written :: ByteLength})
guardByteLength x success =
  if success
    then (\written -> Just {written}) <$> byteLength x
    else pure Nothing
