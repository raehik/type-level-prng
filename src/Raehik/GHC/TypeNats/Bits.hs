{-# LANGUAGE UndecidableInstances #-}

-- | Type-level "Data.Bits" definitions, over GHC opaque type-level 'Natural's.

module Raehik.GHC.TypeNats.Bits where

import GHC.TypeNats
import GHC.TypeError qualified as TE

-- | Left shift.
type ShiftL x i = x * 2^i

-- | Logical right shift. (But these are nats, is arithmetic shiftR even diff??)
type ShiftR x i = x `Div` 2^i

-- | The exclusive or of the binary representation of two 'Natural's.
type Xor n m = XorLoop 1 0 n m

-- calculates one binary digit at a time, so should be O(log n) with value...
-- but with a very high constant factor (8 ops, 2 pattern matches).
-- I would love to compute this more efficiently!
type family XorLoop factor acc n m where
    XorLoop factor acc 0 0 = acc
    XorLoop factor acc n m =
      XorLoop
        (factor*2)
        (acc + factor * (((n `Mod` 2) `BitXor` (m `Mod` 2))))
        (n `Div` 2)
        (m `Div` 2)

-- | Exclusive or on two "bit" 'Natural's.
--
-- Both 'Natural's must be either 0 or 1, or it emits a type error.
type family BitXor n m where
    BitXor 0 0 = 0
    BitXor 0 1 = 1
    BitXor 1 0 = 1
    BitXor 1 1 = 0
    BitXor n m = TE.TypeError (TE.Text "BitXor: got non-bit Naturals")
