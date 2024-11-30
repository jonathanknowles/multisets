module Data.Multiset.Internal where

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

newtype Multiset a = Multiset (MonoidMap a (Sum Natural))
    deriving newtype (Eq)

newtype SignedMultiset a = SignedMultiset (MonoidMap a (Sum Integer))
    deriving newtype (Eq)
