{-# LANGUAGE UndecidableInstances #-}

module TypeLevelPrng.Prng where

import TypeLevelPrng.Common
import GHC.TypeNats
import Raehik.GHC.TypeNats.Bits
import DeFun.Core

{- Here's the problem with PRNGs in type-level Haskell: GHC doesn't afford us
   the fast operations that they rely on. In particular, my XOR implementation
   sucks ass. What to do?

* We could use an LCG. Poor quality randomness, but perhaps good enough?
  What if we use one with a large period, and only provide the top half?
  Then even the low bits have good periods.
  * Oh! Wikipedia even mentions explicitly. A 64-bit LCG returning the top 32
    bits passes SmallCrush.
-}

{- TODO it seems everyone agrees xoshiro256++,** are Good.

-- xoshiro256++ state. n is 64-bit
data State n = State
  { s0 :: n
  , s1 :: n
  , s2 :: n
  , s3 :: n
  }
type PState = State Natural

type Xoshiro256PPNext :: PState -> (PState, Natural)
type family Xoshiro256PPNext n where

type Xoshiro256PPRotl x k = x << k | x >> (64 - k)

{-
static inline uint64_t rotl(const uint64_t x, int k) {
	return (x << k) | (x >> (64 - k));
}

static uint64_t s[4];

uint64_t next(void) {
	const uint64_t result = rotl(s[0] + s[3], 23) + s[0];

	const uint64_t t = s[1] << 17;

	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];

	s[2] ^= t;

	s[3] = rotl(s[3], 45);

	return result;
}
-}

-}
