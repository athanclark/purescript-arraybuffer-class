-- | Newtypes for the various bit-level numeric types.

module Data.ArrayBuffer.Class.Types where

import Prelude (class Eq, class Show)
import Data.UInt (UInt)
import Data.Float32 (Float32)

newtype Uint32BE  = Uint32BE UInt
derive newtype instance eqUint32BE :: Eq Uint32BE
derive newtype instance showUint32BE :: Show Uint32BE
newtype Uint16BE  = Uint16BE UInt
derive newtype instance eqUint32LE :: Eq Uint32LE
derive newtype instance showUint32LE :: Show Uint32LE
newtype Uint32LE  = Uint32LE UInt
derive newtype instance eqUint16BE :: Eq Uint16BE
derive newtype instance showUint16BE :: Show Uint16BE
newtype Uint16LE  = Uint16LE UInt
derive newtype instance eqUint16LE :: Eq Uint16LE
derive newtype instance showUint16LE :: Show Uint16LE
newtype Uint8     = Uint8    UInt
derive newtype instance eqUint8 :: Eq Uint8
derive newtype instance showUint8 :: Show Uint8
newtype Int32BE  = Int32BE Int
derive newtype instance eqInt32BE :: Eq Int32BE
derive newtype instance showInt32BE :: Show Int32BE
newtype Int16BE  = Int16BE Int
derive newtype instance eqInt32LE :: Eq Int32LE
derive newtype instance showInt32LE :: Show Int32LE
newtype Int32LE  = Int32LE Int
derive newtype instance eqInt16BE :: Eq Int16BE
derive newtype instance showInt16BE :: Show Int16BE
newtype Int16LE  = Int16LE Int
derive newtype instance eqInt16LE :: Eq Int16LE
derive newtype instance showInt16LE :: Show Int16LE
newtype Int8     = Int8    Int
derive newtype instance eqInt8 :: Eq Int8
derive newtype instance showInt8 :: Show Int8
newtype Float32BE = Float32BE Float32
derive newtype instance eqFloat32BE :: Eq Float32BE
derive newtype instance showFloat32BE :: Show Float32BE
newtype Float32LE = Float32LE Float32
derive newtype instance eqFloat32LE :: Eq Float32LE
derive newtype instance showFloat32LE :: Show Float32LE
newtype Float64BE = Float64BE Number
derive newtype instance eqFloat64BE :: Eq Float64BE
derive newtype instance showFloat64BE :: Show Float64BE
newtype Float64LE = Float64LE Number
derive newtype instance eqFloat64LE :: Eq Float64LE
derive newtype instance showFloat64LE :: Show Float64LE
