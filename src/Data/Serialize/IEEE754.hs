{-# LANGUAGE FlexibleContexts #-}

-- | Functions for reading, writing and converting IEEE 754 floating
-- point numbers.
--
-- Conversions use 'STUArray' and the 'ST' monad to reinterpret bytes
-- and get /what we assume to be/ the IEEE 754 binary representation
-- of single and double precision floating point numbers.

module Data.Serialize.IEEE754 (

  -- * Big-endian reads
    getFloat32be
  , getFloat64be

  -- * Little-endian reads
  , getFloat32le
  , getFloat64le

  -- * Host-endian, unaligned reads
  , getFloat32host
  , getFloat64host

  -- * Little-endian writes
  , putFloat32le
  , putFloat64le

  -- * Big-endian writes
  , putFloat32be
  , putFloat64be

  -- * Host-endian, unaligned writes
  , putFloat32host
  , putFloat64host

  -- * Conversions
  , floatToWord
  , wordToFloat
  , doubleToWord
  , wordToDouble

  ) where

import Control.Applicative ((<$>))
import Data.Array.ST (newArray, castSTUArray, readArray, MArray, STUArray)
import Data.Serialize
import Data.Word (Word32, Word64)
import GHC.ST (runST, ST)

------------------------------------------------------------------------
-- Get

-- | Read a 32-bit float in little endian format
getFloat32le :: Get Float
getFloat32le = wordToFloat <$> getWord32le

-- | Read a 64-bit float in little endian format
getFloat64le :: Get Double
getFloat64le = wordToDouble <$> getWord64le

-- | Read a 32-bit float in big endian format
getFloat32be :: Get Float
getFloat32be = wordToFloat <$> getWord32be

-- | Read a 64-bit float in big endian format
getFloat64be :: Get Double
getFloat64be = wordToDouble <$> getWord64be

-- | Read a 32-bit float in native host order and host endianness
getFloat32host :: Get Float
getFloat32host = wordToFloat <$> getWord32host

-- | Read a 64-bit float in native host order and host endianness
getFloat64host :: Get Double
getFloat64host = wordToDouble <$> getWord64host

------------------------------------------------------------------------
-- Put

-- | Write a 32-bit float in little endian format
putFloat32le :: Putter Float
putFloat32le = putWord32le . floatToWord

-- | Write a 64-bit float in little endian format
putFloat64le :: Putter Double
putFloat64le = putWord64le . doubleToWord

-- | Write a 32-bit float in big endian format
putFloat32be :: Putter Float
putFloat32be = putWord32be . floatToWord

-- | Write a 64-bit float in big endian format
putFloat64be :: Putter Double
putFloat64be = putWord64be . doubleToWord

-- | Write a 32-bit float in native host order and host endianness
putFloat32host :: Putter Float
putFloat32host = putWord32host . floatToWord

-- | Write a 64-bit float in native host order and host endianness
putFloat64host :: Putter Double
putFloat64host = putWord64host . doubleToWord

------------------------------------------------------------------------
-- Conversions

-- | Interpret a 32-bit word as a 32-bit float.
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

-- | Interpret a 32-bit float as a 32-bit word.
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

-- | Interpret a 64-bit word as a 64-bit float.
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

-- | Interpret a 64-bit float as a 64-bit word.
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
