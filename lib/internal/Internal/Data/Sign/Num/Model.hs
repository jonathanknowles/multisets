module Internal.Data.Sign.Num.Model where

import Internal.Data.Sign.Num
    ( NumSign (N, P, Z)
    , SignNum (signNum)
    )
import Prelude

null :: NumSign -> Bool
null s = toIntegral @Int s == 0

invert :: NumSign -> NumSign
invert s = signNum @Int $ negate $ toIntegral s

add :: NumSign -> NumSign -> NumSign
add s1 s2 =
    signNum @Int $
        (`moduloInclusiveRange` (-1, 1)) $
            toIntegral s1 + toIntegral s2

multiply :: NumSign -> NumSign -> NumSign
multiply s1 s2 =
    signNum @Int $
        toIntegral s1 * toIntegral s2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

moduloInclusiveRange :: Integral i => i -> (i, i) -> i
moduloInclusiveRange i (lo, hi) = ((i - lo) `mod` (hi - lo + 1)) + lo

toIntegral :: Integral i => NumSign -> i
toIntegral = \case
    N -> -1
    Z -> 0
    P -> 1
