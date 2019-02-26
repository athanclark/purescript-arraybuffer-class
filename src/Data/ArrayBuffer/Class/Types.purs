-- | Newtypes for the various bit-level numeric types.

module Data.ArrayBuffer.Class.Types where

import Data.UInt (UInt)
import Data.Float32 (Float32)

newtype Uint32BE  = Uint32BE UInt
newtype Uint16BE  = Uint16BE UInt
newtype Uint32LE  = Uint32LE UInt
newtype Uint16LE  = Uint16LE UInt
newtype Uint8     = Uint8    UInt
newtype Int32BE   = Int32BE  Int
newtype Int16BE   = Int16BE  Int
newtype Int32LE   = Int32LE  Int
newtype Int16LE   = Int16LE  Int
newtype Int8      = Int8     Int
newtype Float32BE = Float32BE Float32
newtype Float32LE = Float32LE Float32
newtype Float64BE = Float64BE Number
newtype Float64LE = Float64LE Number
