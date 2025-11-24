{-# LANGUAGE UndecidableInstances #-}

-- | Xorshift PRNGs.

module Data.TypeLevel.PRNG.Xorshift where

import Data.TypeLevel.PRNG.Common
import GHC.TypeNats
import Raehik.GHC.TypeNats.Bits
import DeFun.Core ( type App )

-- tested a handful of iterations against Wikipedia C program
type Xorshift64 :: SimplePRNG
data Xorshift64 x
type instance App Xorshift64 x = Xorshift64_2 ((x `Xor` (x `ShiftL` 13)) `Mod` (2^64))
type Xorshift64_2 x =            Xorshift64_3  (x `Xor` (x `ShiftR` 7))
type Xorshift64_3 x =                          (x `Xor` (x `ShiftL` 17)) `Mod` (2^64)

-- Seems to be no faster than Xorshift64.
type Xorshift32 :: SimplePRNG
data Xorshift32 x
type instance App Xorshift32 x = Xorshift32_2 ((x `Xor` (x `ShiftL` 13)) `Mod` (2^32))
type Xorshift32_2 x =            Xorshift32_3  (x `Xor` (x `ShiftR` 17))
type Xorshift32_3 x =                          (x `Xor` (x `ShiftL` 5))  `Mod` (2^32)
