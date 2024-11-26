{-# LANGUAGE DeriveFunctor #-}

module Data.Multiset where

import Data.Coerce
    ( coerce
    )
import Data.Foldable qualified as Foldable
import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Data.List
    ( nub
    , subsequences
    )
import Data.Monoid
    ( Sum (Sum)
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
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
    deriving newtype (Eq)

testA :: Multiset Char
testA = fromListWith (+) [('a', 1), ('b', 2), ('c', 3), ('d', 4)]

testB :: Multiset Char
testB = fromListWith (+) [('c', 2), ('d', 4)]

instance Show a => Show (Multiset a) where
    show s = "fromListWith (+) " <> show (toList s)

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

toUnaryList :: Multiset a -> [a]
toUnaryList = foldMap f . toList
  where
    f (_, 0) = []
    f (a, n) = a : f (a, n - 1)

fromUnaryList :: Ord a => [a] -> Multiset a
fromUnaryList = sums . fmap singleton

empty :: Multiset a
empty = Multiset MonoidMap.empty

singleton :: Ord a => a -> Multiset a
singleton a = fromListWith (+) [(a, 1)]

cardinality :: Multiset a -> Natural
cardinality (Multiset s) = coerce (Foldable.fold s)

member :: Ord a => a -> Multiset a -> Bool
member a (Multiset s) = MonoidMap.nonNullKey a s

multiplicity :: Ord a => a -> Multiset a -> Natural
multiplicity a (Multiset s) = coerce (MonoidMap.get a s)

support :: Multiset a -> Set a
support (Multiset s) = MonoidMap.nonNullKeys s

fromSet :: Set a -> Multiset a
fromSet = undefined

isSet :: Multiset a -> Bool
isSet (Multiset s) = Foldable.all (== 1) s

isSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isSubsetOf (Multiset s1) (Multiset s2) = MonoidMap.isSubmapOfBy (<=) s1 s2

isProperSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isProperSubsetOf (Multiset s1) (Multiset s2) = MonoidMap.isSubmapOfBy (<=) s1 s2

powerset :: Ord a => Multiset a -> [Multiset a]
powerset = fmap fromUnaryList . nub . subsequences . toUnaryList

powersetSize :: Ord a => Multiset a -> Natural
powersetSize (Multiset s) =
    coerce $ Foldable.foldl' (\x y -> x * (y + 1)) (Sum 1) s

disjoint :: Ord a => Multiset a -> Multiset a -> Bool
disjoint s1 s2 = Set.disjoint (support s1) (support s2)

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (Multiset s1) (Multiset s2) =
    Multiset $ s1 `MonoidMap.monus` s2

differenceMaybe :: Ord a => Multiset a -> Multiset a -> Maybe (Multiset a)
differenceMaybe (Multiset s1) (Multiset s2) =
    Multiset <$> s1 `MonoidMap.minusMaybe` s2

-- consider having a single monoid (analogous to Sum Natural) where
-- <> = sum
-- lcm = union
-- gcd = intersection
-- <\> = difference
-- </> = differenceMaybe
--
-- See: https://en.wikipedia.org/wiki/Multiset

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
