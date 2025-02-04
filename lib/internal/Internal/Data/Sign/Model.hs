module Internal.Data.Sign.Model where

import Internal.Data.Sign.Num
    ( NumSign (..)
    )
import Internal.Data.Sign.Num qualified as NumSign
import Prelude

null :: NumSign -> Bool
null s = signToIntegral @Int s == 0

invert :: NumSign -> NumSign
invert s = NumSign.signOf @Int $ negate $ signToIntegral s

add :: NumSign -> NumSign -> NumSign
add s1 s2 =
    NumSign.signOf @Int $
        (`moduloInclusiveRange` (-1, 1)) $
            signToIntegral s1 + signToIntegral s2

multiply :: NumSign -> NumSign -> NumSign
multiply s1 s2 =
    NumSign.signOf @Int $
        signToIntegral s1 * signToIntegral s2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

moduloInclusiveRange :: Integral i => i -> (i, i) -> i
moduloInclusiveRange i (lo, hi) = ((i - lo) `mod` (hi - lo + 1)) + lo

signToIntegral :: Integral i => NumSign -> i
signToIntegral = \case
    N -> -1
    Z -> 0
    P -> 1
