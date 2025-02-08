module Examples.PrimeFactorisation where

import Data.Bag
    ( Bag
    )
import Data.Bag qualified as Bag
import Data.Bag.Signed
    ( SignedBag
    )
import Data.Bag.Signed qualified as SignedBag
import Data.Ratio
    ( Ratio
    , denominator
    , numerator
    )
import Math.NumberTheory.Primes
    ( Prime
    )
import Math.NumberTheory.Primes qualified as Primes
import Numeric.Natural
    ( Natural
    )
import Prelude

factoriseNatural :: Natural -> Bag (Prime Natural)
factoriseNatural =
    Bag.fromList
        . fmap (fmap (fromIntegral @Word @Natural))
        . Primes.factorise

factoriseRational :: Ratio Natural -> SignedBag (Prime Natural)
factoriseRational r =
    SignedBag.add
        ( SignedBag.fromList
            . fmap (fmap wordToPositiveInteger)
            . Primes.factorise
            $ numerator r
        )
        ( SignedBag.fromList
            . fmap (fmap wordToNegativeInteger)
            . Primes.factorise
            $ denominator r
        )
  where
    wordToPositiveInteger = fromIntegral @Word @Integer
    wordToNegativeInteger = negate . fromIntegral @Word @Integer
