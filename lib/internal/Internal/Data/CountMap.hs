module Internal.Data.CountMap where

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
import Data.Group
    ( Group
    )
import Data.Map.Strict
    ( Map
    )
import Data.Map.Strict qualified as Map
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Semiring
    ( Semiring (one)
    )
import Data.Set
    ( Set
    )
import Data.Set qualified as Set
import Internal.Data.Count
    ( Count (Count)
    , CountMagnitude (countToNatural)
    )
import Internal.Data.Packed
    ( Packed (Unpacked, pack, unpack)
    , unpacked
    , unpacked2
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( sum
    )

type CountMap a c = MonoidMap a (Count c)

type PackedCountMap p k c = (Packed p, Unpacked p ~ CountMap k c)

showWith
    :: PackedCountMap p k c
    => Show k
    => Show c
    => String -> String -> p -> String
showWith typeName operatorName m =
    typeName <> ".fromListWith " <> operatorName <> " " <> show (toList m)

empty :: PackedCountMap p k c => p
empty = pack MonoidMap.empty

singleton
    :: PackedCountMap p k c
    => Ord k
    => MonoidNull (Count c)
    => Semiring (Count c)
    => k -> p
singleton k = pack $ MonoidMap.singleton k one

fromListWith
    :: PackedCountMap p k c
    => Ord k
    => MonoidNull (Count c)
    => (c -> c -> c)
    -> [(k, c)]
    -> p
fromListWith f xs = pack $ MonoidMap.fromListWith (coerce f) (coerce xs)

toList :: forall p k c. PackedCountMap p k c => p -> [(k, c)]
toList = coerce @[(k, Count c)] @[(k, c)] . MonoidMap.toList . unpack

fromMap
    :: forall p k c
     . PackedCountMap p k c
    => MonoidNull (Count c)
    => Map k c -> p
fromMap = pack . MonoidMap.fromMap . coerce @(Map k c) @(Map k (Count c))

toMap :: forall p k c. PackedCountMap p k c => p -> Map k c
toMap = coerce @(Map k (Count c)) @(Map k c) . MonoidMap.toMap . unpack

null :: PackedCountMap p k c => p -> Bool
null = MonoidMap.null . unpack

lookup
    :: PackedCountMap p k c
    => Ord k
    => Monoid (Count c)
    => k -> p -> c
lookup k = unpack . MonoidMap.get k . unpack

member
    :: PackedCountMap p k c
    => Ord k
    => k -> p -> Bool
member k = MonoidMap.nonNullKey k . unpack

invert
    :: PackedCountMap p a c
    => MonoidNull (Count c)
    => Group (Count c)
    => p -> p
invert = unpacked MonoidMap.invert

intersection
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => p -> p -> p
intersection = unpacked2 (MonoidMap.unionWith min)

union
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => p -> p -> p
union = unpacked2 (MonoidMap.unionWith max)

sum
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => p -> p -> p
sum = unpacked2 (MonoidMap.unionWith (<>))

intersections1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
intersections1 = Foldable1.foldl1' intersection

unions
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Ord k
    => Ord c
    => Foldable f
    => f p -> p
unions = Foldable.foldl' union empty

unions1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
unions1 = Foldable1.foldl1' union

sums
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Ord k
    => Ord c
    => Foldable f
    => f p -> p
sums = Foldable.foldl' sum empty

minus
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Group (Count c)
    => Ord k
    => p -> p -> p
minus = unpacked2 MonoidMap.minus

monus
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Monus (Count c)
    => Ord k
    => p -> p -> p
monus = unpacked2 MonoidMap.monus

symmetricDifference
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Monus (Count c)
    => Ord k
    => p -> p -> p
symmetricDifference =
    unpacked2 $
        MonoidMap.unionWith $
            \c1 c2 -> (c1 <\> c2) <> (c2 <\> c1)

-- Note: evaluation will terminate early if (and only if) the maps are
-- incomparable.
compare
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Maybe Ordering
compare s1 s2 = go False False (compareElements s1 s2)
{- ORMOLU_DISABLE -}
  where
    go    True    True              _ = Nothing
    go    True   False             [] = Just LT
    go   False    True             [] = Just GT
    go   False   False             [] = Just EQ
    go _seenLT  seenGT ((_, LT) : xs) = go True   seenGT xs
    go  seenLT _seenGT ((_, GT) : xs) = go seenLT   True xs
    go  seenLT  seenGT ((_, EQ) : xs) = go seenLT seenGT xs
{- ORMOLU_ENABLE -}

compareElements
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> [(k, Ordering)]
compareElements s1 s2 = fmap (uncurry Prelude.compare) <$> align s1 s2

-- Note: evaluation will terminate early if (and only if) a GT is detected.
isLessThan
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isLessThan m1 m2 = go False (compareElements m1 m2)
{- ORMOLU_DISABLE -}
  where
    go seenLT             [] = seenLT
    go _      ((_, LT) : xs) = go True   xs
    go seenLT ((_, EQ) : xs) = go seenLT xs
    go _      ((_, GT) :  _) = False
{- ORMOLU_ENABLE -}

-- Note: evaluation will terminate early if (and only if) a LT is detected.
isGreaterThan
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isGreaterThan m1 m2 = go False (compareElements m1 m2)
{- ORMOLU_DISABLE -}
  where
    go seenGT             [] = seenGT
    go _      ((_, LT) :  _) = False
    go seenGT ((_, EQ) : xs) = go seenGT xs
    go _      ((_, GT) : xs) = go True   xs
{- ORMOLU_ENABLE -}

isLessThanOrEqualTo
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isLessThanOrEqualTo m1 m2 = GT `notElem` (snd <$> compareElements m1 m2)

isGreaterThanOrEqualTo
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isGreaterThanOrEqualTo s1 s2 = LT `notElem` (snd <$> compareElements s1 s2)

isSubmapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSubmapOf m1 m2 =
    Map.isSubmapOfBy (<=) (toMap m1) (toMap m2)

isProperSubmapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSubmapOf m1 m2 =
    Map.isProperSubmapOfBy (<=) (toMap m1) (toMap m2)

isSupermapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSupermapOf m1 m2 =
    Map.isSubmapOfBy (<=) (toMap m2) (toMap m1)

isProperSupermapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSupermapOf m1 m2 =
    Map.isProperSubmapOfBy (<=) (toMap m2) (toMap m1)

isSymmetricSubmapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSymmetricSubmapOf m1 m2 =
    Map.isSubmapOfBy isBoundedBy (toMap m1) (toMap m2)

isProperSymmetricSubmapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSymmetricSubmapOf m1 m2 =
    Map.isProperSubmapOfBy isBoundedBy (toMap m1) (toMap m2)

isSymmetricSupermapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSymmetricSupermapOf m1 m2 =
    Map.isSubmapOfBy isBoundedBy (toMap m2) (toMap m1)

isProperSymmetricSupermapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSymmetricSupermapOf m1 m2 =
    Map.isProperSubmapOfBy isBoundedBy (toMap m2) (toMap m1)

-- The set of all subsets.
powerset
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Ord p
    => Ord k
    => Enum (Count c)
    => p -> Set p
powerset = Set.fromList . powersetElements

-- Generates all subsets in lexicograhic order.
powersetElements
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Ord k
    => Enum (Count c)
    => p -> [p]
{- ORMOLU_DISABLE -}
powersetElements =
    fmap (pack . MonoidMap.fromListWith (<>)) . go . MonoidMap.toList . unpack
  where
    go            [] = [[]]
    go ((a, p) : xs) = [(a, q) : ys | q <- shrinkInclusive p, ys <- go xs]

    shrinkInclusive :: (Enum a, Monoid a) => a -> [a]
    shrinkInclusive a = [mempty .. a]
{- ORMOLU_ENABLE -}

powersetSize
    :: PackedCountMap p k c
    => CountMagnitude c
    => PositiveMonoid (Count c)
    => p
    -> Natural
powersetSize m =
    Foldable.foldl'
        (\x y -> x * (countToNatural y + 1))
        1
        (unpack m)

-- The set of all symmetric subsets.
symmetricPowerset
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Group (Count c)
    => Ord p
    => Ord k
    => Ord c
    => Enum (Count c)
    => p -> Set p
symmetricPowerset = Set.fromList . symmetricPowersetElements

-- Generates all symmetric subsets in lexicograhic order.
symmetricPowersetElements
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Group (Count c)
    => Ord k
    => Ord c
    => Enum (Count c)
    => p -> [p]
{- ORMOLU_DISABLE -}
symmetricPowersetElements =
    fmap (pack . MonoidMap.fromListWith (<>)) . go . MonoidMap.toList . unpack
  where
    go            [] = [[]]
    go ((a, p) : xs) = [(a, q) : ys | q <- shrinkInclusive p, ys <- go xs]

    shrinkInclusive :: (Enum a, Monoid a, Ord a) => a -> [a]
    shrinkInclusive a
        | a < mempty = [a .. mempty]
        | a > mempty = [mempty .. a]
        | otherwise  = [mempty]
{- ORMOLU_ENABLE -}

symmetricPowersetSize
    :: PackedCountMap p k c
    => CountMagnitude c
    => Group (Count c)
    => p
    -> Natural
symmetricPowersetSize m =
    Foldable.foldl'
        (\x y -> x * (countToNatural y + 1))
        1
        (unpack m)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

align
    :: PackedCountMap p k c
    => Ord k
    => Monoid (Count c)
    => p
    -> p
    -> [(k, (Count c, Count c))]
{- ORMOLU_DISABLE -}
align = go `on` (coerce . toList)
  where
    go            []            [] = []
    go ((a, p) : xs)            [] = (a, (p, z)) : go xs []
    go            [] ((b, q) : ys) = (b, (z, q)) : go [] ys
    go ((a, p) : xs) ((b, q) : ys)
        | a < b                    = (a, (p, z)) : go           xs ((b, q) : ys)
        | a > b                    = (b, (z, q)) : go ((a, p) : xs)          ys
        | otherwise                = (a, (p, q)) : go           xs           ys

    z = mempty
{- ORMOLU_ENABLE -}

isBoundedBy :: (Ord a, Monoid (Count a)) => a -> a -> Bool
isBoundedBy v1 v2
    | Count v1 <= mempty && Count v2 <= mempty = v1 >= v2
    | Count v1 >= mempty && Count v2 >= mempty = v1 <= v2
    | otherwise = False
