{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Multiset where

import Data.Coerce
    ( coerce
    )
import Data.Foldable qualified as Foldable
import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Data.Function
    ( on
    )
import Data.Map.Strict
    ( Map
    )
import Data.Map.Strict qualified as Map
import Data.Monoid
    ( Sum (Sum)
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Multiset.Internal
    ( Multiset (Multiset)
    , SignedMultiset (SignedMultiset)
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

instance Ord a => Ord (Multiset a) where
    compare = Prelude.compare `on` toMap

instance Show a => Show (Multiset a) where
    show s = "Multiset.fromListWith (+) " <> show (toList s)

testA :: Multiset Char
testA = fromListWith (+) [('a', 1), ('b', 2), ('c', 3), ('d', 4)]

testB :: Multiset Char
testB = fromListWith (+) [('c', 2), ('d', 4)]

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

fromMap :: Map a Natural -> Multiset a
fromMap = Multiset . MonoidMap.fromMap . coerce

toMap :: Multiset a -> Map a Natural
toMap (Multiset s) = coerce (MonoidMap.toMap s)

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

invert :: Multiset a -> SignedMultiset a
invert = undefined

fromSet :: Set a -> Multiset a
fromSet = fromSetWith (const 1)

fromSetWith :: (a -> Natural) -> Set a -> Multiset a
fromSetWith f = Multiset . MonoidMap.fromMap . Map.fromSet (coerce f)

-- Caution: this function will only short-circuit if the sets are incomparable.
--
{- ORMOLU_DISABLE -}
compare :: Ord a => Multiset a -> Multiset a -> Maybe Ordering
compare s1 s2 = go False False (compareAll s1 s2)
  where
    go    True    True              _ = Nothing
    go    True   False             [] = Just LT
    go   False    True             [] = Just GT
    go   False   False             [] = Just EQ
    go _seenLT  seenGT ((_, LT) : xs) = go True   seenGT xs
    go  seenLT _seenGT ((_, GT) : xs) = go seenLT   True xs
    go  seenLT  seenGT ((_, EQ) : xs) = go seenLT seenGT xs
{- ORMOLU_ENABLE -}

compareAll :: Ord a => Multiset a -> Multiset a -> [(a, Ordering)]
compareAll s1 s2 = fmap (uncurry Prelude.compare) <$> align s1 s2

isSet :: Multiset a -> Bool
isSet (Multiset s) = Foldable.all (== 1) s

isSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isSubsetOf = Map.isSubmapOfBy (<=) `on` toMap

isProperSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isProperSubsetOf = Map.isProperSubmapOfBy (<=) `on` toMap

isSupersetOf :: Ord a => Multiset a -> Multiset a -> Bool
isSupersetOf = flip isSubsetOf

isProperSupersetOf :: Ord a => Multiset a -> Multiset a -> Bool
isProperSupersetOf = flip isProperSubsetOf

-- The set of all subsets.
powerset :: Ord a => Multiset a -> Set (Multiset a)
powerset = Set.fromList . powersetElements

-- Generates all subsets in lexicograhic order.
{- ORMOLU_DISABLE -}
powersetElements :: Ord a => Multiset a -> [Multiset a]
powersetElements = fmap (fromListWith (+)) . go . toList
  where
    go            [] = [[]]
    go ((a, p) : xs) = [(a, q) : ys | q <- shrinkInclusive p, ys <- go xs]

    shrinkInclusive :: Natural -> [Natural]
    shrinkInclusive a = [0 .. a]
{- ORMOLU_ENABLE -}

powersetSize :: Ord a => Multiset a -> Natural
powersetSize (Multiset s) =
    coerce $ Foldable.foldl' (\x y -> x * (y + 1)) (Sum 1) s

disjoint :: Ord a => Multiset a -> Multiset a -> Bool
disjoint (Multiset s1) (Multiset s2) = MonoidMap.disjoint s1 s2

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (Multiset s1) (Multiset s2) =
    Multiset $ s1 `MonoidMap.monus` s2

differenceMaybe :: Ord a => Multiset a -> Multiset a -> Maybe (Multiset a)
differenceMaybe (Multiset s1) (Multiset s2) =
    Multiset <$> s1 `MonoidMap.minusMaybe` s2

differenceSigned :: Ord a => Multiset a -> Multiset a -> SignedMultiset a
differenceSigned (Multiset s1) (Multiset s2) =
    SignedMultiset $
        MonoidMap.unionWith ((-) `on` coerce naturalToPositiveInteger) s1 s2

symmetricDifference
    :: Ord a
    => Multiset a
    -> Multiset a
    -> Multiset a
symmetricDifference (Multiset s1) (Multiset s2) =
    Multiset $ MonoidMap.unionWith (coerce naturalDistance) s1 s2

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
align
    :: Ord a
    => Multiset a
    -> Multiset a
    -> [(a, (Natural, Natural))]
align = go `on` toList
  where
    go            []            [] = []
    go ((a, p) : xs)            [] = (a, (p, 0)) : go xs []
    go            [] ((b, q) : ys) = (b, (0, q)) : go [] ys
    go ((a, p) : xs) ((b, q) : ys)
        | a < b                    = (a, (p, 0)) : go           xs ((b, q) : ys)
        | a > b                    = (b, (0, q)) : go ((a, p) : xs)          ys
        | otherwise                = (a, (p, q)) : go           xs           ys
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
naturalDistance :: Natural -> Natural -> Natural
naturalDistance a b
    | a > b     = a - b
    | otherwise = b - a
{- ORMOLU_ENABLE -}

naturalToPositiveInteger :: Natural -> Integer
naturalToPositiveInteger = fromIntegral
