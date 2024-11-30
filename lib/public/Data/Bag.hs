{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Bag
    ( Bag
    , fromListWith
    , toList
    , invert
    , powersetElements
    )
where

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
import Data.Bag.Internal
    ( Bag (Bag)
    , SignedBag (SignedBag)
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

instance Ord a => Ord (Bag a) where
    compare = Prelude.compare `on` toMap

instance Show a => Show (Bag a) where
    show s = "Bag.fromListWith (+) " <> show (toList s)

testA :: Bag Char
testA = fromListWith (+) [('a', 1), ('b', 2), ('c', 3), ('d', 4)]

testB :: Bag Char
testB = fromListWith (+) [('c', 2), ('d', 4)]

fromListWith
    :: Ord a
    => (Natural -> Natural -> Natural)
    -> [(a, Natural)]
    -> Bag a
fromListWith f =
    Bag
        . MonoidMap.fromListWith (coerce f)
        . coerce

toList :: Bag a -> [(a, Natural)]
toList (Bag s) = coerce (MonoidMap.toList s)

toUnaryList :: Bag a -> [a]
toUnaryList = foldMap f . toList
  where
    f (_, 0) = []
    f (a, n) = a : f (a, n - 1)

fromUnaryList :: Ord a => [a] -> Bag a
fromUnaryList = sums . fmap singleton

fromMap :: Map a Natural -> Bag a
fromMap = Bag . MonoidMap.fromMap . coerce

toMap :: Bag a -> Map a Natural
toMap (Bag s) = coerce (MonoidMap.toMap s)

empty :: Bag a
empty = Bag MonoidMap.empty

singleton :: Ord a => a -> Bag a
singleton a = fromListWith (+) [(a, 1)]

cardinality :: Bag a -> Natural
cardinality (Bag s) = coerce (Foldable.fold s)

member :: Ord a => a -> Bag a -> Bool
member a (Bag s) = MonoidMap.nonNullKey a s

multiplicity :: Ord a => a -> Bag a -> Natural
multiplicity a (Bag s) = coerce (MonoidMap.get a s)

support :: Bag a -> Set a
support (Bag s) = MonoidMap.nonNullKeys s

invert :: Bag a -> SignedBag a
invert = undefined

fromSet :: Set a -> Bag a
fromSet = fromSetWith (const 1)

fromSetWith :: (a -> Natural) -> Set a -> Bag a
fromSetWith f = Bag . MonoidMap.fromMap . Map.fromSet (coerce f)

-- Caution: this function will only short-circuit if the sets are incomparable.
--
{- ORMOLU_DISABLE -}
compare :: Ord a => Bag a -> Bag a -> Maybe Ordering
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

compareAll :: Ord a => Bag a -> Bag a -> [(a, Ordering)]
compareAll s1 s2 = fmap (uncurry Prelude.compare) <$> align s1 s2

isSet :: Bag a -> Bool
isSet (Bag s) = Foldable.all (== 1) s

isSubsetOf :: Ord a => Bag a -> Bag a -> Bool
isSubsetOf = Map.isSubmapOfBy (<=) `on` toMap

isProperSubsetOf :: Ord a => Bag a -> Bag a -> Bool
isProperSubsetOf = Map.isProperSubmapOfBy (<=) `on` toMap

isSupersetOf :: Ord a => Bag a -> Bag a -> Bool
isSupersetOf = flip isSubsetOf

isProperSupersetOf :: Ord a => Bag a -> Bag a -> Bool
isProperSupersetOf = flip isProperSubsetOf

-- The set of all subsets.
powerset :: Ord a => Bag a -> Set (Bag a)
powerset = Set.fromList . powersetElements

-- Generates all subsets in lexicograhic order.
{- ORMOLU_DISABLE -}
powersetElements :: Ord a => Bag a -> [Bag a]
powersetElements = fmap (fromListWith (+)) . go . toList
  where
    go            [] = [[]]
    go ((a, p) : xs) = [(a, q) : ys | q <- shrinkInclusive p, ys <- go xs]

    shrinkInclusive :: Natural -> [Natural]
    shrinkInclusive a = [0 .. a]
{- ORMOLU_ENABLE -}

powersetSize :: Ord a => Bag a -> Natural
powersetSize (Bag s) =
    coerce $ Foldable.foldl' (\x y -> x * (y + 1)) (Sum 1) s

disjoint :: Ord a => Bag a -> Bag a -> Bool
disjoint (Bag s1) (Bag s2) = MonoidMap.disjoint s1 s2

difference :: Ord a => Bag a -> Bag a -> Bag a
difference (Bag s1) (Bag s2) =
    Bag $ s1 `MonoidMap.monus` s2

differenceMaybe :: Ord a => Bag a -> Bag a -> Maybe (Bag a)
differenceMaybe (Bag s1) (Bag s2) =
    Bag <$> s1 `MonoidMap.minusMaybe` s2

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

-- consider having a single monoid (analogous to Sum Natural) where
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

union :: Ord a => Bag a -> Bag a -> Bag a
union (Bag s1) (Bag s2) =
    Bag $ MonoidMap.unionWith max s1 s2

unions :: Foldable f => Ord a => f (Bag a) -> Bag a
unions = Foldable.foldl' union empty

intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection (Bag s1) (Bag s2) =
    Bag $ MonoidMap.intersectionWith min s1 s2

intersections :: Foldable1 f => Ord a => f (Bag a) -> Bag a
intersections = Foldable1.foldl1' intersection

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
align
    :: Ord a
    => Bag a
    -> Bag a
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
