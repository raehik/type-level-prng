module Data.TypeLevel.PRNG.Common
  ( type PRNG, type SimplePRNG, type FromSimplePRNG
  ) where

import GHC.TypeNats ( type Natural )
import DeFun.Core ( type App, type (@@), type (~>) )

-- | A PRNG takes some state, and returns a new state and some random data.
--
-- The characteristics of the 'Natural' data returned are dependent on the PRNG.
type PRNG state = state ~> (state, Natural)

-- | A simple PRNG where the state is the previous output.
type SimplePRNG = Natural ~> Natural

-- | Turn a simple PRNG into a regular one.
type FromSimplePRNG :: SimplePRNG -> PRNG Natural
data FromSimplePRNG prng n
type instance App (FromSimplePRNG prng) n = Dup (prng @@ n)
-- @(prng @@ n, prng @@ n)@ probably re-does the computation, so.

type Dup :: a -> (a, a)
type Dup a = '(a, a)
