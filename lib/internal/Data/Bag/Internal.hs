module Data.Bag.Internal where

import Data.Coerce
    ( coerce
    )
import Data.Function
    ( on
    )
import Data.Map.Strict
    ( Map
    )
import Data.Monoid
    ( Sum (Sum)
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Bag a = Bag (MonoidMap a (Sum Natural))
    deriving newtype (Eq)

newtype SignedBag a = SignedBag (MonoidMap a (Sum Integer))
    deriving newtype (Eq)

instance Ord a => Ord (Bag a) where
    compare (Bag b1) (Bag b2) =
        (compare `on` toMap) b1 b2

instance Ord a => Ord (SignedBag a) where
    compare (SignedBag b1) (SignedBag b2) =
        (compare `on` toMap) b1 b2

instance Show a => Show (Bag a) where
    show (Bag s) =
        "Bag.fromListWith (+) "
            <> show (toList s)

instance Show a => Show (SignedBag a) where
    show (SignedBag s) =
        "SignedBag.fromListWith (+) "
            <> show (toList s)

toList :: forall a n. MonoidMap a (Sum n) -> [(a, n)]
toList =
    coerce
        @(_ -> [(a, Sum n)])
        @(_ -> [(a, n)])
        MonoidMap.toList

toMap :: forall a n. MonoidMap a (Sum n) -> Map a n
toMap =
    coerce
        @(_ -> Map a (Sum n))
        @(_ -> Map a n)
        MonoidMap.toMap
