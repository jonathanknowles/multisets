module Data.Multiset where

import Prelude hiding
    ( sum )

import Data.MonoidMap
    ( MonoidMap )
import Numeric.Natural
    ( Natural )
import Data.Coerce
    ( coerce )
import Data.Multiset.Combinators
    ( Sum (..), Union (..) )

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Monoid
    ( Sum (Sum) )

newtype Multiset v = Multiset (MonoidMap v (Data.Monoid.Sum Natural))

instance Show v => Show (Multiset v) where
    show s = "fromListSum " <> show (toList s)

instance Ord v => Semigroup (Sum (Multiset v)) where
    (<>) = coerce sum
instance Ord v => Monoid (Sum (Multiset v)) where
    mempty = coerce empty

instance Ord v => Semigroup (Union (Multiset v)) where
    (<>) = coerce union
instance Ord v => Monoid (Union (Multiset v)) where
    mempty = coerce empty

fromListSum :: Ord v => [(v, Natural)] -> Multiset v
fromListSum = fromListWith (+)

fromListUnion :: Ord v => [(v, Natural)] -> Multiset v
fromListUnion = fromListWith max

fromListWith
    :: Ord v
    => (Natural -> Natural -> Natural)
    -> [(v, Natural)]
    -> Multiset v
fromListWith f
    = Multiset
    . MonoidMap.fromListWith (coerce f)
    . fmap (fmap Data.Monoid.Sum)

toList :: Multiset v -> [(v, Natural)]
toList (Multiset s) = coerce (MonoidMap.toList s)

empty :: Multiset v
empty = Multiset MonoidMap.empty

difference :: Ord v => Multiset v -> Multiset v -> Multiset v
difference (Multiset m1) (Multiset m2) =
    Multiset $ m1 `MonoidMap.monus` m2

sum :: Ord v => Multiset v -> Multiset v -> Multiset v
sum (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.unionWith (+) m1 m2

union :: Ord v => Multiset v -> Multiset v -> Multiset v
union (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.unionWith max m1 m2

intersection :: Ord v => Multiset v -> Multiset v -> Multiset v
intersection (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.intersectionWith min m1 m2
