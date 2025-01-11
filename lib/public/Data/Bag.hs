module Data.Bag
    ( -- * Type
      Bag

      -- * Construction
    , empty
    , singleton
    , fromList
    , fromListWith
    , fromMap
    , fromSet

      -- * Deconstruction
    , toList
    , toMap
    , toSet

      -- * Membership
    , null
    , count
    , member

      -- * Indication
    , isRegular
    , isSimple

      -- * Folding
    , foldl
    , foldl'
    , foldr
    , foldr'
    , foldMap
    , foldMap'

      -- * Mapping
    , map
    , mapWith
    , mapCounts

      -- * Algebra
    , add
    , addMany
    , union
    , unionMany
    , intersection
    , intersectionMany1
    , difference
    , symmetricDifference

      -- * Comparison
    , compareLexically
    , isLessThan
    , isLessThanOrEqualTo
    , isGreaterThan
    , isGreaterThanOrEqualTo
    , isSubbagOf
    , isSuperbagOf
    , isProperSubbagOf
    , isProperSuperbagOf

      -- * Combinatorics
    , powerset
    , powersetElements
    , powersetSize
    )
where

import Data.Foldable1
    ( Foldable1
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Shared
    ( Bag
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( foldMap
    , foldl
    , foldl'
    , foldr
    , map
    , null
    , sum
    )

empty :: Bag a
empty = CountMap.empty

singleton :: Ord a => a -> Bag a
singleton = CountMap.singleton

fromList :: Ord a => [(a, Natural)] -> Bag a
fromList = CountMap.fromList

fromListWith
    :: Ord a
    => (Natural -> Natural -> Natural)
    -> [(a, Natural)]
    -> Bag a
fromListWith = CountMap.fromListWith

fromMap :: Map a Natural -> Bag a
fromMap = CountMap.fromMap

fromSet :: (a -> Natural) -> Set a -> Bag a
fromSet = CountMap.fromSet

toList :: Bag a -> [(a, Natural)]
toList = CountMap.toList

toMap :: Bag a -> Map a Natural
toMap = CountMap.toMap

toSet :: Bag a -> Set a
toSet = CountMap.toSet

null :: Bag a -> Bool
null = CountMap.null

count :: Ord a => a -> Bag a -> Natural
count = CountMap.count

member :: Ord a => a -> Bag a -> Bool
member = CountMap.member

isRegular :: Bag a -> Bool
isRegular = CountMap.isRegular

isSimple :: Bag a -> Bool
isSimple = CountMap.isSimple

foldl :: (r -> a -> Natural -> r) -> r -> Bag a -> r
foldl = CountMap.foldl

foldl' :: (r -> a -> Natural -> r) -> r -> Bag a -> r
foldl' = CountMap.foldl'

foldr :: (a -> Natural -> r -> r) -> r -> Bag a -> r
foldr = CountMap.foldr

foldr' :: (a -> Natural -> r -> r) -> r -> Bag a -> r
foldr' = CountMap.foldr'

foldMap :: Monoid m => (a -> Natural -> m) -> Bag a -> m
foldMap = CountMap.foldMap

foldMap' :: Monoid m => (a -> Natural -> m) -> Bag a -> m
foldMap' = CountMap.foldMap'

map :: Ord b => (a -> b) -> Bag a -> Bag b
map = CountMap.map

mapWith
    :: Ord b
    => (Natural -> Natural -> Natural)
    -> (a -> b)
    -> Bag a
    -> Bag b
mapWith = CountMap.mapWith

mapCounts :: (Natural -> Natural) -> Bag a -> Bag a
mapCounts = CountMap.mapCounts

add :: Ord a => Bag a -> Bag a -> Bag a
add = CountMap.add

addMany :: Foldable f => Ord a => f (Bag a) -> Bag a
addMany = CountMap.addMany

union :: Ord a => Bag a -> Bag a -> Bag a
union = CountMap.union

unionMany :: Foldable f => Ord a => f (Bag a) -> Bag a
unionMany = CountMap.unionMany

intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection = CountMap.intersection

intersectionMany1 :: Foldable1 f => Ord a => f (Bag a) -> Bag a
intersectionMany1 = CountMap.intersectionMany1

difference :: Ord a => Bag a -> Bag a -> Bag a
difference = CountMap.monus

symmetricDifference :: Ord a => Bag a -> Bag a -> Bag a
symmetricDifference = CountMap.symmetricDifference

compareLexically :: Ord a => Bag a -> Bag a -> Ordering
compareLexically = CountMap.compareLexically

isLessThan :: Ord a => Bag a -> Bag a -> Bool
isLessThan = CountMap.isLessThan

isLessThanOrEqualTo :: Ord a => Bag a -> Bag a -> Bool
isLessThanOrEqualTo = CountMap.isLessThanOrEqualTo

isGreaterThan :: Ord a => Bag a -> Bag a -> Bool
isGreaterThan = CountMap.isGreaterThan

isGreaterThanOrEqualTo :: Ord a => Bag a -> Bag a -> Bool
isGreaterThanOrEqualTo = CountMap.isGreaterThanOrEqualTo

isSubbagOf :: Ord a => Bag a -> Bag a -> Bool
isSubbagOf = CountMap.isSubmapOf

isSuperbagOf :: Ord a => Bag a -> Bag a -> Bool
isSuperbagOf = CountMap.isSupermapOf

isProperSubbagOf :: Ord a => Bag a -> Bag a -> Bool
isProperSubbagOf = CountMap.isProperSubmapOf

isProperSuperbagOf :: Ord a => Bag a -> Bag a -> Bool
isProperSuperbagOf = CountMap.isProperSupermapOf

powerset :: Ord a => Bag a -> Set (Bag a)
powerset = CountMap.powerset

powersetElements :: Ord a => Bag a -> [Bag a]
powersetElements = CountMap.powersetElements

powersetSize :: Ord a => Bag a -> Natural
powersetSize = CountMap.powersetSize

{-

testA :: Bag Char
testA = fromListWith (+) [('a', 1), ('b', 2), ('c', 3), ('d', 4)]

testB :: Bag Char
testB = fromListWith (+) [('c', 2), ('d', 4)]

toUnaryList :: Bag a -> [a]
toUnaryList = foldMap f . toList
  where
    f (_, 0) = []
    f (a, n) = a : f (a, n - 1)

fromUnaryList :: Ord a => [a] -> Bag a
fromUnaryList = sums . fmap singleton

singleton :: Ord a => a -> Bag a
singleton a = fromListWith (+) [(a, 1)]

cardinality :: Bag a -> N
cardinality (Bag s) = coerce (Foldable.fold s)

member :: Ord a => a -> Bag a -> Bool
member a (Bag s) = MonoidMap.nonNullKey a s

-- height (greatest)
-- depth (least)
-- isWholeSubset (contains all multiplicities of the common objects)
-- isFullSubset (supports are the same)
-- powermultiset - multiset of all submultisets
-- dominates - Knuth
-- is this a semiring?
-- products?
-- https://quivergeometry.net/multisets/

multiplicity :: Ord a => a -> Bag a -> N
multiplicity a (Bag s) = coerce (MonoidMap.get a s)

support :: Bag a -> Set a
support (Bag s) = MonoidMap.nonNullKeys s

fromSet :: Set a -> Bag a
fromSet = fromSetWith (const 1)

fromSetWith :: (a -> N) -> Set a -> Bag a
fromSetWith f = Bag . MonoidMap.fromMap . Map.fromSet (coerce f)

isSet :: Bag a -> Bool
isSet (Bag s) = Foldable.all (== 1) s

disjoint :: Ord a => Bag a -> Bag a -> Bool
disjoint = undefined -- (Bag s1) (Bag s2) = MonoidMap.disjoint s1 s2

difference :: Ord a => Bag a -> Bag a -> Bag a
difference (Bag s1) (Bag s2) =
    undefined -- Bag $ s1 `MonoidMap.monus` s2

differenceMaybe :: Ord a => Bag a -> Bag a -> Maybe (Bag a)
differenceMaybe (Bag s1) (Bag s2) =
    undefined -- Bag <$> s1 `MonoidMap.minusMaybe` s2

differenceSigned :: Ord a => Bag a -> Bag a -> SignedBag a
differenceSigned (Bag s1) (Bag s2) =
    SignedBag $
        MonoidMap.unionWith ((-) `on` coerce naturalToPositiveInteger) s1 s2

symmetricDifference
    :: Ord a
    => Bag a
    -> Bag a
    -> Bag a
symmetricDifference (Bag s1) (Bag s2) =
    Bag $ MonoidMap.unionWith (coerce naturalDistance) s1 s2

-- consider having a single monoid (analogous to Count N) where
-- <> = sum
-- lcm = union
-- gcd = intersection
-- <\> = difference
-- </> = differenceMaybe
--
-- See: https://en.wikipedia.org/wiki/Bag

sum :: Ord a => Bag a -> Bag a -> Bag a
sum (Bag s1) (Bag s2) =
    Bag $ MonoidMap.unionWith (+) s1 s2

sums :: Foldable f => Ord a => f (Bag a) -> Bag a
sums = Foldable.foldl' sum empty

-}
