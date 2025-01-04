{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Bag
    ( Bag
    , empty
    , singleton
    , fromListWith
    , fromMap
    , toList
    , toMap
    , null
    , lookup
    , member
    , union
    , unions
    , intersection
    , intersections1
    , difference
    , symmetricDifference
    , isLessThan
    , isLessThanOrEqualTo
    , isGreaterThan
    , isGreaterThanOrEqualTo
    , isSubbagOf
    , isSuperbagOf
    , isProperSubbagOf
    , isProperSuperbagOf
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
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( lookup
    , null
    , sum
    )
import qualified Internal.Data.CountMap as CountMap
import Internal.Shared
    ( Bag )

type N = Natural

empty :: Bag a
empty = CountMap.empty

singleton :: Ord a => a -> Bag a
singleton = CountMap.singleton

fromListWith :: Ord a => (N -> N -> N) -> [(a, N)] -> Bag a
fromListWith = CountMap.fromListWith

fromMap :: Map a N -> Bag a
fromMap = CountMap.fromMap

toList :: Bag a -> [(a, N)]
toList = CountMap.toList

toMap :: Bag a -> Map a N
toMap = CountMap.toMap

null :: Bag a -> Bool
null = CountMap.null

lookup :: Ord a => a -> Bag a -> N
lookup = CountMap.lookup

member :: Ord a => a -> Bag a -> Bool
member = CountMap.member

union :: Ord a => Bag a -> Bag a -> Bag a
union = CountMap.union

unions :: Foldable f => Ord a => f (Bag a) -> Bag a
unions = CountMap.unions

intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection = CountMap.intersection

intersections1 :: Foldable1 f => Ord a => f (Bag a) -> Bag a
intersections1 = CountMap.intersections1

difference :: Ord a => Bag a -> Bag a -> Bag a
difference = CountMap.monus

symmetricDifference :: Ord a => Bag a -> Bag a -> Bag a
symmetricDifference = CountMap.symmetricDifference

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

powersetElements :: Ord a => Bag a -> [Bag a]
powersetElements = CountMap.powersetElements

powersetSize :: Ord a => Bag a -> N
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
-- isRegular (all objects have same multiplicity
-- isSimple (all objects are the same)
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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
naturalDistance :: N -> N -> N
naturalDistance a b
    | a > b     = a - b
    | otherwise = b - a
{- ORMOLU_ENABLE -}

naturalToPositiveInteger :: N -> Integer
naturalToPositiveInteger = fromIntegral

-}
