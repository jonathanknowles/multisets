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
import Data.Set
    ( Set
    )
import Data.Set qualified as Set
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( sum
    )

newtype Multiset a = Multiset (MonoidMap a (Data.Monoid.Sum Natural))
    deriving (Eq)

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

cardinality :: Multiset a -> Natural
cardinality (Multiset s) = coerce (Foldable.fold s)

member :: Ord a => a -> Multiset a -> Bool
member a (Multiset s) = MonoidMap.nonNullKey a s

multiplicity :: Ord a => a -> Multiset a -> Natural
multiplicity a (Multiset s) = coerce (MonoidMap.get a s)

support :: Multiset a -> Set a
support (Multiset s) = MonoidMap.nonNullKeys s

powerSet :: Multiset a -> Set (Multiset a)
powerSet = undefined

fromSet :: Set a -> Multiset a
fromSet = undefined

isSet :: Multiset a -> Bool
isSet (Multiset s) = Foldable.all (== 1) s

isSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isSubsetOf = undefined

isProperSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isProperSubsetOf = undefined

disjoint :: Ord a => Multiset a -> Multiset a -> Bool
disjoint s1 s2 = Set.disjoint (support s1) (support s2)

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (Multiset s1) (Multiset s2) =
    Multiset $ s1 `MonoidMap.monus` s2

differenceMaybe :: Ord a => Multiset a -> Multiset a -> Maybe (Multiset a)
differenceMaybe (Multiset s1) (Multiset s2) =
    Multiset <$> s1 `MonoidMap.minusMaybe` s2

sum :: Ord a => Multiset a -> Multiset a -> Multiset a
sum (Multiset s1) (Multiset s2) =
    Multiset $ MonoidMap.unionWith (+) s1 s2

sums :: Foldable f => Ord a => f (Multiset a) -> Multiset a
sums = Foldable.foldl' sum empty

union :: Ord a => Multiset a -> Multiset a -> Multiset a
union (Multiset s1) (Multiset s2) =
    Multiset $ MonoidMap.unionWith max s1 s2

unions :: Foldable f => Ord a => f (Multiset a) -> Multiset a
unions = Foldable.foldl' union empty

intersection :: Ord a => Multiset a -> Multiset a -> Multiset a
intersection (Multiset s1) (Multiset s2) =
    Multiset $ MonoidMap.intersectionWith min s1 s2

intersections :: Foldable1 f => Ord a => f (Multiset a) -> Multiset a
intersections = Foldable1.foldl1' intersection
