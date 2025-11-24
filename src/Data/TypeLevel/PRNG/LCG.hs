{-# LANGUAGE UndecidableInstances #-}

-- | Linear congruential generators (and Lehmer RNGs).

module Data.TypeLevel.PRNG.LCG where

import Data.TypeLevel.PRNG.Common
import GHC.TypeNats
import DeFun.Core ( type App )

-- TODO are the constants re-calculated each usage?
-- should I calculate @2^31 - 1@ and insert manually? benchmark.

-- | Calculate the next term in a linear congruential generator.
--
-- The previous term is given in @n@.
type LCGNext :: Natural -> Natural -> Natural -> SimplePRNG
data LCGNext m a c n
type instance App (LCGNext m a c) n = (a * n + c) `Mod` m

-- | Knuth's MMIX RNG. An LCG.
--
-- As I understand, the bits have a period of 2^n where n is their bit position.
-- So the low bits have very low randomness: e.g. it flips between odd & even!
-- Consider only using the top 32 bits.
type MMIX = LCGNext (2^64) 6364136223846793005 1442695040888963407

-- | Calculate the next term in a Lehmer random number generator (a type of
--   linear congruential generator). A type of LCG.
--
-- The previous term is given in @n@.
--
-- Very few operations.
type LehmerNext :: Natural -> Natural -> SimplePRNG
data LehmerNext m a n
type instance App (LehmerNext m a) n = (a * n) `Mod` m

-- | minstd_rand from C++11. A Lehmer RNG.
--
-- Seems pretty good. Low bits aren't as low period like many other LCGs.
type MinstdRand = LehmerNext (2^31 - 1) 48271
