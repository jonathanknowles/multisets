module Data.Multiset where

import Data.Coerce
    ( coerce
    )
import Data.Foldable qualified as Foldable
import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Data.Monoid qualified
    ( Sum (Sum)
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Multiset.Combinators
    ( Intersection (Intersection)
    , Sum (Sum)
    , Union (Union)
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( sum
    )

newtype Multiset a = Multiset (MonoidMap a (Data.Monoid.Sum Natural))

instance Show a => Show (Multiset a) where
    show s = "fromListWith (+) " <> show (toList s)

instance Ord a => Semigroup (Sum (Multiset a)) where
    (<>) = coerce sum

instance Ord a => Monoid (Sum (Multiset a)) where
    mempty = coerce empty

instance Ord a => Semigroup (Union (Multiset a)) where
    (<>) = coerce union

instance Ord a => Monoid (Union (Multiset a)) where
    mempty = coerce empty

instance Ord a => Semigroup (Intersection (Multiset a)) where
    (<>) = coerce union

fromListWith
    :: Ord a
    => (Natural -> Natural -> Natural)
    -> [(a, Natural)]
    -> Multiset a
fromListWith f =
    Multiset
        . MonoidMap.fromListWith (coerce f)
        . coerce

toList :: Multiset a -> [(a, Natural)]
toList (Multiset s) = coerce (MonoidMap.toList s)

empty :: Multiset a
empty = Multiset MonoidMap.empty

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (Multiset m1) (Multiset m2) =
    Multiset $ m1 `MonoidMap.monus` m2

sum :: Ord a => Multiset a -> Multiset a -> Multiset a
sum (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.unionWith (+) m1 m2

sums :: Foldable f => Ord a => f (Multiset a) -> Multiset a
sums = Foldable.foldl' sum empty

union :: Ord a => Multiset a -> Multiset a -> Multiset a
union (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.unionWith max m1 m2

unions :: Foldable f => Ord a => f (Multiset a) -> Multiset a
unions = Foldable.foldl' union empty

intersection :: Ord a => Multiset a -> Multiset a -> Multiset a
intersection (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.intersectionWith min m1 m2

intersections :: Foldable1 f => Ord a => f (Multiset a) -> Multiset a
intersections = Foldable1.foldl1' intersection
