module Internal.Data.Sign.Model where

import Internal.Data.Sign
    ( Sign (..)
    )
import Internal.Data.Sign qualified as Sign
import Prelude

null :: Sign -> Bool
null s = signToIntegral @Int s == 0

invert :: Sign -> Sign
invert s = Sign.signOf @Int $ negate $ signToIntegral s

add :: Sign -> Sign -> Sign
add s1 s2 =
    Sign.signOf @Int $
        (`moduloInclusiveRange` (-1, 1)) $
            signToIntegral s1 + signToIntegral s2

multiply :: Sign -> Sign -> Sign
multiply s1 s2 =
    Sign.signOf @Int $
        signToIntegral s1 * signToIntegral s2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

moduloInclusiveRange :: Integral i => i -> (i, i) -> i
moduloInclusiveRange i (lo, hi) = ((i - lo) `mod` (hi - lo + 1)) + lo

signToIntegral :: Integral i => Sign -> i
signToIntegral = \case
    N -> -1
    Z -> 0
    P -> 1
