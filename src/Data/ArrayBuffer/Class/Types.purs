-- | Newtypes for the various bit-level numeric types.

module Data.ArrayBuffer.Class.Types where

import Prelude
  ( class Eq, class Ord, class Show, class Semiring, class Ring, class CommutativeRing
  , class EuclideanRing, class DivisionRing)
import Data.UInt (UInt)
import Data.Float32 (Float32)

newtype Uint32BE  = Uint32BE UInt
derive newtype instance eqUint32BE :: Eq Uint32BE
derive newtype instance ordUint32BE :: Ord Uint32BE
derive newtype instance showUint32BE :: Show Uint32BE
derive newtype instance semiringUint32BE :: Semiring Uint32BE
derive newtype instance ringUint32BE :: Ring Uint32BE
derive newtype instance commutativeRingUint32BE :: CommutativeRing Uint32BE
derive newtype instance euclideanRingUint32BE :: EuclideanRing Uint32BE
newtype Uint32LE  = Uint32LE UInt
derive newtype instance eqUint32LE :: Eq Uint32LE
derive newtype instance ordUint32LE :: Ord Uint32LE
derive newtype instance showUint32LE :: Show Uint32LE
derive newtype instance semiringUint32LE :: Semiring Uint32LE
derive newtype instance ringUint32LE :: Ring Uint32LE
derive newtype instance commutativeRingUint32LE :: CommutativeRing Uint32LE
derive newtype instance euclideanRingUint32LE :: EuclideanRing Uint32LE
newtype Uint16BE  = Uint16BE UInt
derive newtype instance eqUint16BE :: Eq Uint16BE
derive newtype instance ordUint16BE :: Ord Uint16BE
derive newtype instance showUint16BE :: Show Uint16BE
derive newtype instance semiringUint16BE :: Semiring Uint16BE
derive newtype instance ringUint16BE :: Ring Uint16BE
derive newtype instance commutativeRingUint16BE :: CommutativeRing Uint16BE
derive newtype instance euclideanRingUint16BE :: EuclideanRing Uint16BE
newtype Uint16LE  = Uint16LE UInt
derive newtype instance eqUint16LE :: Eq Uint16LE
derive newtype instance ordUint16LE :: Ord Uint16LE
derive newtype instance showUint16LE :: Show Uint16LE
derive newtype instance semiringUint16LE :: Semiring Uint16LE
derive newtype instance ringUint16LE :: Ring Uint16LE
derive newtype instance commutativeRingUint16LE :: CommutativeRing Uint16LE
derive newtype instance euclideanRingUint16LE :: EuclideanRing Uint16LE
newtype Uint8     = Uint8    UInt
derive newtype instance eqUint8 :: Eq Uint8
derive newtype instance ordUint8 :: Ord Uint8
derive newtype instance showUint8 :: Show Uint8
derive newtype instance semiringUint8 :: Semiring Uint8
derive newtype instance ringUint8 :: Ring Uint8
derive newtype instance commutativeRingUint8 :: CommutativeRing Uint8
derive newtype instance euclideanRingUint8 :: EuclideanRing Uint8
newtype Int32BE  = Int32BE Int
derive newtype instance eqInt32BE :: Eq Int32BE
derive newtype instance ordInt32BE :: Ord Int32BE
derive newtype instance showInt32BE :: Show Int32BE
derive newtype instance semiringInt32BE :: Semiring Int32BE
derive newtype instance ringInt32BE :: Ring Int32BE
derive newtype instance commutativeRingInt32BE :: CommutativeRing Int32BE
derive newtype instance euclideanRingInt32BE :: EuclideanRing Int32BE
newtype Int32LE  = Int32LE Int
derive newtype instance eqInt32LE :: Eq Int32LE
derive newtype instance ordInt32LE :: Ord Int32LE
derive newtype instance showInt32LE :: Show Int32LE
derive newtype instance semiringInt32LE :: Semiring Int32LE
derive newtype instance ringInt32LE :: Ring Int32LE
derive newtype instance commutativeRingInt32LE :: CommutativeRing Int32LE
derive newtype instance euclideanRingInt32LE :: EuclideanRing Int32LE
newtype Int16BE  = Int16BE Int
derive newtype instance eqInt16BE :: Eq Int16BE
derive newtype instance ordInt16BE :: Ord Int16BE
derive newtype instance showInt16BE :: Show Int16BE
derive newtype instance semiringInt16BE :: Semiring Int16BE
derive newtype instance ringInt16BE :: Ring Int16BE
derive newtype instance commutativeRingInt16BE :: CommutativeRing Int16BE
derive newtype instance euclideanRingInt16BE :: EuclideanRing Int16BE
newtype Int16LE  = Int16LE Int
derive newtype instance eqInt16LE :: Eq Int16LE
derive newtype instance ordInt16LE :: Ord Int16LE
derive newtype instance showInt16LE :: Show Int16LE
derive newtype instance semiringInt16LE :: Semiring Int16LE
derive newtype instance ringInt16LE :: Ring Int16LE
derive newtype instance commutativeRingInt16LE :: CommutativeRing Int16LE
derive newtype instance euclideanRingInt16LE :: EuclideanRing Int16LE
newtype Int8     = Int8    Int
derive newtype instance eqInt8 :: Eq Int8
derive newtype instance ordInt8 :: Ord Int8
derive newtype instance showInt8 :: Show Int8
derive newtype instance semiringInt8 :: Semiring Int8
derive newtype instance ringInt8 :: Ring Int8
derive newtype instance commutativeRingInt8 :: CommutativeRing Int8
derive newtype instance euclideanRingInt8 :: EuclideanRing Int8
newtype Float32BE = Float32BE Float32
derive newtype instance eqFloat32BE :: Eq Float32BE
derive newtype instance ordFloat32BE :: Ord Float32BE
derive newtype instance showFloat32BE :: Show Float32BE
derive newtype instance semiringFloat32BE :: Semiring Float32BE
derive newtype instance ringFloat32BE :: Ring Float32BE
derive newtype instance commutativeRingFloat32BE :: CommutativeRing Float32BE
derive newtype instance euclideanRingFloat32BE :: EuclideanRing Float32BE
derive newtype instance divisionRingFloat32BE :: DivisionRing Float32BE
newtype Float32LE = Float32LE Float32
derive newtype instance eqFloat32LE :: Eq Float32LE
derive newtype instance ordFloat32LE :: Ord Float32LE
derive newtype instance showFloat32LE :: Show Float32LE
derive newtype instance semiringFloat32LE :: Semiring Float32LE
derive newtype instance ringFloat32LE :: Ring Float32LE
derive newtype instance commutativeRingFloat32LE :: CommutativeRing Float32LE
derive newtype instance euclideanRingFloat32LE :: EuclideanRing Float32LE
derive newtype instance divisionRingFloat32LE :: DivisionRing Float32LE
newtype Float64BE = Float64BE Number
derive newtype instance eqFloat64BE :: Eq Float64BE
derive newtype instance ordFloat64BE :: Ord Float64BE
derive newtype instance showFloat64BE :: Show Float64BE
derive newtype instance semiringFloat64BE :: Semiring Float64BE
derive newtype instance ringFloat64BE :: Ring Float64BE
derive newtype instance commutativeRingFloat64BE :: CommutativeRing Float64BE
derive newtype instance euclideanRingFloat64BE :: EuclideanRing Float64BE
derive newtype instance divisionRingFloat64BE :: DivisionRing Float64BE
newtype Float64LE = Float64LE Number
derive newtype instance eqFloat64LE :: Eq Float64LE
derive newtype instance ordFloat64LE :: Ord Float64LE
derive newtype instance showFloat64LE :: Show Float64LE
derive newtype instance semiringFloat64LE :: Semiring Float64LE
derive newtype instance ringFloat64LE :: Ring Float64LE
derive newtype instance commutativeRingFloat64LE :: CommutativeRing Float64LE
derive newtype instance euclideanRingFloat64LE :: EuclideanRing Float64LE
derive newtype instance divisionRingFloat64LE :: DivisionRing Float64LE
