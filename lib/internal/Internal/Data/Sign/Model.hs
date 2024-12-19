module Internal.Data.Sign.Model where

import Internal.Data.Sign
    ( Sign (..)
    )
import Internal.Data.Sign qualified as Sign
import Prelude

null :: Sign -> Bool
null s = signToIntegral @Int s == 0

invert :: Sign -> Sign
invert s = Sign.fromIntegral @Int $ negate $ signToIntegral s

{- ORMOLU_DISABLE -}
add :: Sign -> Sign -> Sign
add s1 s2
    = Sign.fromIntegral @Int
    $ (`moduloInclusiveRange` (-1, 1))
    $ signToIntegral s1 + signToIntegral s2
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
multiply :: Sign -> Sign -> Sign
multiply s1 s2
    = Sign.fromIntegral @Int
    $ signToIntegral s1 * signToIntegral s2
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

moduloInclusiveRange :: Integral i => i -> (i, i) -> i
moduloInclusiveRange i (lo, hi) = ((i - lo) `mod` (hi - lo + 1)) + lo

{- ORMOLU_DISABLE -}
signToIntegral :: Integral i => Sign -> i
signToIntegral = \case
    Negative -> -1
    Zero     ->  0
    Positive ->  1
{- ORMOLU_ENABLE -}
