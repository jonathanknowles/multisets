module Data.Bag.Internal where

import Data.Monoid
    ( Sum
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Bag a = Bag (MonoidMap a (Sum Natural))
    deriving newtype (Eq)

newtype SignedBag a = SignedBag (MonoidMap a (Sum Integer))
    deriving newtype (Eq)
