module Internal.Data.CountMap where

-- TODO:
--
-- fromListUnary
-- fromListWith
--
-- add
-- filter
-- partition
-- restrictKeys?
-- withoutKeys?

import Data.Coerce
    ( coerce
    )
import Data.Foldable qualified as Foldable
import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Data.Function
    ( (&)
    )
import Data.Group
    ( Group
    )
import Data.Map.Strict
    ( Map
    )
import Data.Map.Strict qualified as Map
import Data.Maybe
    ( isJust
    , mapMaybe
    )
import Data.Monoid.Monus
    ( Monus
    )
import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.Monoid.Null qualified as Null
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Ord
    ( Down (Down)
    )
import Data.Semiring
    ( Ring
    , Semiring
    )
import Data.Semiring qualified as Ring
import Data.Semiring qualified as Semiring
import Data.Set
    ( Set
    )
import Data.Set qualified as Set
import Internal.Data.Count
    ( Count (Count)
    , CountMagnitude (countToNatural)
    , CountSymmetricDifference
        ( CountSymmetricDifferenceAbsolute
        , countSymmetricDifference
        , countSymmetricDifferenceAbsolute
        )
    )
import Internal.Data.Magnitude
    ( HasMagnitude (Magnitude, magnitude)
    )
import Internal.Data.Packed
    ( Packed (Unpacked, pack, unpack)
    , unpacked
    , unpacked2
    )
import Internal.Data.Sign
    ( Sign (Negative, Positive)
    , SignOrZero
    , Signed (SignOf, signOf)
    )
import Internal.Data.Sign qualified as Sign
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( foldl'
    , foldr
    , lookup
    , map
    , max
    , min
    , null
    , sum
    )
import Prelude qualified as Prelude

type CountMap a c = MonoidMap a (Count c)

type PackedCountMap p k c = (Packed p, Unpacked p ~ CountMap k c)

type Naked c = c

showFromList
    :: PackedCountMap p k c
    => Show k
    => Show c
    => String -> p -> String
showFromList typeName m =
    typeName <> ".fromList " <> Prelude.show (toList m)

showFromListWith
    :: PackedCountMap p k c
    => Show k
    => Show c
    => String -> String -> p -> String
showFromListWith typeName operatorName m =
    typeName <> ".fromListWith " <> operatorName <> " " <> show (toList m)

empty :: PackedCountMap p k c => p
empty = pack MonoidMap.empty

singleton
    :: PackedCountMap p k c
    => Ord k
    => MonoidNull (Count c)
    => Semiring (Count c)
    => k -> p
singleton k = pack $ MonoidMap.singleton k Semiring.one

fromList
    :: forall p k c
     . PackedCountMap p k c
    => Ord k
    => MonoidNull (Count c)
    => [(k, c)]
    -> p
fromList =
    fromListWith $
        coerce
            @(Count c -> Count c -> Count c)
            @(Naked c -> Naked c -> Naked c)
            mappend

fromListWith
    :: PackedCountMap p k c
    => Ord k
    => MonoidNull (Count c)
    => (c -> c -> c)
    -> [(k, c)]
    -> p
fromListWith f xs = pack $ MonoidMap.fromListWith (coerce f) (coerce xs)

toList :: forall p k c. PackedCountMap p k c => p -> [(k, c)]
toList =
    coerce
        @[(k, Count c)]
        @[(k, Naked c)]
        . MonoidMap.toList
        . unpack

toListAsc :: forall p k c. PackedCountMap p k c => p -> [(k, c)]
toListAsc = Map.toAscList . toMap

toListDesc :: forall p k c. PackedCountMap p k c => p -> [(k, c)]
toListDesc = Map.toDescList . toMap

fromMap
    :: forall p k c
     . PackedCountMap p k c
    => MonoidNull (Count c)
    => Map k c -> p
fromMap =
    pack
        . MonoidMap.fromMap
        . coerce
            @(Map k (Naked c))
            @(Map k (Count c))

toMap :: forall p k c. PackedCountMap p k c => p -> Map k c
toMap =
    coerce
        @(Map k (Count c))
        @(Map k (Naked c))
        . MonoidMap.toMap
        . unpack

toSet :: PackedCountMap p k c => p -> Set k
toSet p = MonoidMap.nonNullKeys (unpack p)

toSetSigned
    :: PackedCountMap p1 k c1
    => PackedCountMap p2 k SignOrZero
    => Signed c1
    => SignOf c1 ~ SignOrZero
    => Num c1
    => Ord c1
    => p1 -> p2
toSetSigned = fromMap . Map.map signOf . toMap

fromSet
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => (k -> c)
    -> Set k
    -> p
fromSet f s = pack $ MonoidMap.fromSet (coerce f) s

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

signs
    :: PackedCountMap p k c
    => Signed c
    => SignOf c ~ SignOrZero
    => p -> Set Sign
signs =
    Set.fromList
        . mapMaybe (Sign.assertNonZero . signOf . snd)
        . toList

isRegular
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> Bool
isRegular = isJust . maybeRegular

isSimple
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> Bool
isSimple = isJust . maybeSimple

isSingleton
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Semiring c
    => Eq c
    => p -> Bool
isSingleton = isJust . maybeSingleton

isSingletonSigned
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ring (Count c)
    => Eq c
    => p -> Bool
isSingletonSigned = isJust . maybeSingletonSigned

isBipolar
    :: forall p k c
     . ()
    => PackedCountMap p k c
    => Signed c
    => SignOf c ~ SignOrZero
    => p -> Bool
isBipolar p = Set.size (signs p) == 2

isUnipolar
    :: forall p k c
     . ()
    => PackedCountMap p k c
    => Signed c
    => SignOf c ~ SignOrZero
    => p -> Bool
isUnipolar p = Set.size (signs p) == 1

isNegative
    :: forall p k c
     . ()
    => PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => p -> Bool
isNegative p = Foldable.all (<= mempty) (unpack p)

isPositive
    :: forall p k c
     . ()
    => PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => p -> Bool
isPositive p = Foldable.all (>= mempty) (unpack p)

maybeRegular
    :: forall p k c
     . PackedCountMap p k c
    => Monoid (Count c)
    => Ord k
    => Ord c
    => p
    -> Maybe (c, Set k)
{- ORMOLU_DISABLE -}
maybeRegular p =
    -- TODO:
    -- Optimise this function, so that instead of constructing the entire set
    -- of unique counts and then testing its size, we terminate as soon as we
    -- detect more than one unique count.
    case Set.toList uniqueCounts of
        -- TODO: Consider whether or not to return 'Nothing' here:
        [ ] -> Just (z, Set.empty)
        [c] -> Just (c,   toSet p)
        (_) -> Nothing
  where
    z :: c
    z = coerce (mempty :: Count c)

    uniqueCounts :: Set c
    uniqueCounts = Set.fromList $ fmap snd $ toList p
{- ORMOLU_ENABLE -}

maybeSimple
    :: forall p k c
     . ()
    => PackedCountMap p k c
    => Monoid (Count c)
    => Ord k
    => Ord c
    => p
    -> Maybe (c, k)
maybeSimple p =
    -- TODO:
    -- Optimise this function, so that instead of constructing the entire set
    -- of unique keys and then testing its size, we terminate as soon as we
    -- detect more than one unique key.
    case Set.toList uniqueKeys of
        [k] -> Just (lookup k p, k)
        (_) -> Nothing
  where
    uniqueKeys :: Set k
    uniqueKeys = toSet p

maybeSingleton
    :: forall p k c
     . ()
    => PackedCountMap p k c
    => Monoid (Count c)
    => Semiring (Count c)
    => Eq c
    => p
    -> Maybe k
maybeSingleton p =
    case MonoidMap.toList (unpack p) of
        [(k, c)] | c == Semiring.one -> Just k
        _ -> Nothing

maybeSingletonSigned
    :: forall p k c
     . ()
    => PackedCountMap p k c
    => Monoid (Count c)
    => Ring (Count c)
    => Eq c
    => p
    -> Maybe (Sign, k)
maybeSingletonSigned p =
    case MonoidMap.toList (unpack p) of
        [(k, c)] | c == positiveOne -> Just (Positive, k)
        [(k, c)] | c == negativeOne -> Just (Negative, k)
        _ -> Nothing
  where
    positiveOne = Semiring.one
    negativeOne = Semiring.one & Ring.negate

maybeUnipolar
    :: forall p1 p2 k c1 c2
     . ()
    => PackedCountMap p1 k c1
    => PackedCountMap p2 k c2
    => HasMagnitude c1
    => Signed c1
    => SignOf c1 ~ SignOrZero
    => Magnitude c1 ~ c2
    => MonoidNull (Count c1)
    => MonoidNull (Count c2)
    => Ord c1
    => p1 -> Maybe (Sign, p2)
{- ORMOLU_DISABLE -}
maybeUnipolar p =
    case Set.toList (signs p) of
        [s] -> Just (s, fromMap $ Map.map magnitude $ toMap p)
        (_) -> Nothing
{- ORMOLU_ENABLE -}

maybeNegative
    :: forall p1 p2 k c1 c2
     . ()
    => PackedCountMap p1 k c1
    => PackedCountMap p2 k c2
    => HasMagnitude c1
    => Magnitude c1 ~ c2
    => MonoidNull (Count c1)
    => MonoidNull (Count c2)
    => Ord c1
    => p1 -> Maybe p2
maybeNegative p =
    if Foldable.all (<= mempty) (unpack p)
        then Just $ fromMap $ Map.map magnitude $ toMap p
        else Nothing

maybePositive
    :: forall p1 p2 k c1 c2
     . ()
    => PackedCountMap p1 k c1
    => PackedCountMap p2 k c2
    => HasMagnitude c1
    => Magnitude c1 ~ c2
    => MonoidNull (Count c1)
    => MonoidNull (Count c2)
    => Ord c1
    => p1 -> Maybe p2
maybePositive p =
    if Foldable.all (>= mempty) (unpack p)
        then Just $ fromMap $ Map.map magnitude $ toMap p
        else Nothing

foldRoots
    :: PackedCountMap p k c
    => Monoid k
    => p -> k
foldRoots = Foldable.fold . toSet

foldRoots'
    :: PackedCountMap p k c
    => Monoid k
    => p -> k
foldRoots' = Foldable.foldMap' id . toSet

foldlRoots
    :: PackedCountMap p k c
    => (r -> k -> r)
    -> r
    -> p
    -> r
foldlRoots f r p = Foldable.foldl f r (toSet p)

foldlRoots'
    :: PackedCountMap p k c
    => (r -> k -> r)
    -> r
    -> p
    -> r
foldlRoots' f r p = Foldable.foldl' f r (toSet p)

foldrRoots
    :: PackedCountMap p k c
    => (k -> r -> r)
    -> r
    -> p
    -> r
foldrRoots f r p = Foldable.foldr f r (toSet p)

foldrRoots'
    :: PackedCountMap p k c
    => (k -> r -> r)
    -> r
    -> p
    -> r
foldrRoots' f r p = Foldable.foldr' f r (toSet p)

foldMapRoots
    :: PackedCountMap p k c
    => Monoid m
    => (k -> m)
    -> p
    -> m
foldMapRoots f = Foldable.foldMap f . toSet

foldMapRoots'
    :: PackedCountMap p k c
    => Monoid m
    => (k -> m)
    -> p
    -> m
foldMapRoots' f = Foldable.foldMap' f . toSet

foldl
    :: PackedCountMap p k c
    => (r -> k -> c -> r)
    -> r
    -> p
    -> r
foldl f r p = MonoidMap.foldlWithKey (coerce f) r (unpack p)

foldl'
    :: PackedCountMap p k c
    => (r -> k -> c -> r)
    -> r
    -> p
    -> r
foldl' f r p = MonoidMap.foldlWithKey' (coerce f) r (unpack p)

foldr
    :: PackedCountMap p k c
    => (k -> c -> r -> r)
    -> r
    -> p
    -> r
foldr f r p = MonoidMap.foldrWithKey (coerce f) r (unpack p)

foldr'
    :: PackedCountMap p k c
    => (k -> c -> r -> r)
    -> r
    -> p
    -> r
foldr' f r p = MonoidMap.foldrWithKey' (coerce f) r (unpack p)

foldMap
    :: PackedCountMap p k c
    => Monoid m
    => (k -> c -> m)
    -> p
    -> m
foldMap f p = MonoidMap.foldMapWithKey (coerce f) (unpack p)

foldMap'
    :: PackedCountMap p k c
    => Monoid m
    => (k -> c -> m)
    -> p
    -> m
foldMap' f p = MonoidMap.foldMapWithKey' (coerce f) (unpack p)

map
    :: PackedCountMap p1 k1 c
    => PackedCountMap p2 k2 c
    => Ord k2
    => MonoidNull (Count c)
    => (k1 -> k2)
    -> (p1 -> p2)
map f = unpacked (MonoidMap.mapKeysWith (<>) f)

mapWith
    :: PackedCountMap p1 k1 c
    => PackedCountMap p2 k2 c
    => Ord k2
    => MonoidNull (Count c)
    => (c -> c -> c)
    -> (k1 -> k2)
    -> (p1 -> p2)
mapWith f g = unpacked (MonoidMap.mapKeysWith (coerce f) g)

mapCounts
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => (c -> c)
    -> (p -> p)
mapCounts f = unpacked (MonoidMap.map (coerce f))

invert
    :: PackedCountMap p a c
    => MonoidNull (Count c)
    => Group (Count c)
    => p -> p
invert = unpacked MonoidMap.invert

add
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => Semiring c
    => p -> p -> p
add = unpacked2 $ MonoidMap.unionWith Semiring.plus

multiply
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => Semiring c
    => p -> p -> p
multiply = unpacked2 $ MonoidMap.unionWith Semiring.times

intersection
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => p -> p -> p
intersection = unpacked2 $ MonoidMap.unionWith Prelude.min

union
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => p -> p -> p
union = unpacked2 $ MonoidMap.unionWith Prelude.max

supportiveIntersection
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => p -> p -> p
supportiveIntersection = unpacked2 $ MonoidMap.intersectionWith Prelude.min

supportiveUnion
    :: PackedCountMap p k c
    => Ord k
    => Ord c
    => MonoidNull (Count c)
    => p -> p -> p
supportiveUnion = unpacked2 $ MonoidMap.unionWith f
  where
    f c1 c2
        | Null.null c1 = c2
        | Null.null c2 = c1
        | otherwise = Prelude.max c1 c2

addMany
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Semiring c
    => Ord k
    => Ord c
    => Foldable f
    => f p -> p
addMany = Foldable.foldl' add empty

multiplyMany1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Semiring c
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
multiplyMany1 = Foldable1.foldl1' multiply

intersectionMany1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
intersectionMany1 = Foldable1.foldl1' intersection

unionMany
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Ord k
    => Ord c
    => Foldable f
    => f p -> p
unionMany = Foldable.foldl' union empty

unionMany1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
unionMany1 = Foldable1.foldl1' union

supportiveIntersectionMany1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
supportiveIntersectionMany1 = Foldable1.foldl1' supportiveIntersection

supportiveUnionMany
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Ord k
    => Ord c
    => Foldable f
    => f p -> p
supportiveUnionMany = Foldable.foldl' supportiveUnion empty

supportiveUnionMany1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
supportiveUnionMany1 = Foldable1.foldl1' supportiveUnion

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
    => CountSymmetricDifference c
    => Ord k
    => p -> p -> p
symmetricDifference =
    unpacked2 $ MonoidMap.unionWith countSymmetricDifference

symmetricDifferenceAbsolute
    :: PackedCountMap p k c
    => PackedCountMap q k d
    => CountSymmetricDifference c
    => CountSymmetricDifferenceAbsolute c ~ d
    => MonoidNull (Count c)
    => MonoidNull (Count d)
    => Ord k
    => p -> p -> q
symmetricDifferenceAbsolute =
    unpacked2 $ MonoidMap.unionWith countSymmetricDifferenceAbsolute

-- Note: evaluation will terminate early if (and only if) the maps are
-- incomparable.
compareMaybe
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Maybe Ordering
compareMaybe s1 s2 = go False False (compareElementsAsc s1 s2)
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

compareElementsAsc
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> [(k, Ordering)]
compareElementsAsc s1 s2 = fmap (uncurry Prelude.compare) <$> alignAsc s1 s2

compareElementsDesc
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> [(k, Ordering)]
compareElementsDesc s1 s2 = fmap (uncurry Prelude.compare) <$> alignDesc s1 s2

compareLexically
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord k
    => Ord c
    => p -> p -> Ordering
compareLexically = compareLexicallyWith alignAsc

compareColexically
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord k
    => Ord c
    => p -> p -> Ordering
compareColexically = compareLexicallyWith alignDesc

compareLexicallyWith
    :: Ord c
    => (p -> p -> [(k, (c, c))])
    -> p
    -> p
    -> Ordering
compareLexicallyWith align p1 p2 = go (align p1 p2)
  where
    go [] = EQ
    go ((_, (c1, c2)) : kcs)
        | c1 < c2 = LT
        | c1 > c2 = GT
        | otherwise = go kcs

isSubmapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSubmapOf m1 m2 = GT `notElem` (snd <$> compareElementsAsc m1 m2)

isSupermapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSupermapOf s1 s2 = LT `notElem` (snd <$> compareElementsAsc s1 s2)

-- Note: evaluation will terminate early if (and only if) a GT is detected.
isProperSubmapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSubmapOf m1 m2 = go False (compareElementsAsc m1 m2)
{- ORMOLU_DISABLE -}
  where
    go seenLT             [] = seenLT
    go _      ((_, LT) : xs) = go True   xs
    go seenLT ((_, EQ) : xs) = go seenLT xs
    go _      ((_, GT) :  _) = False
{- ORMOLU_ENABLE -}

-- Note: evaluation will terminate early if (and only if) a LT is detected.
isProperSupermapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSupermapOf m1 m2 = go False (compareElementsAsc m1 m2)
{- ORMOLU_DISABLE -}
  where
    go seenGT             [] = seenGT
    go _      ((_, LT) :  _) = False
    go seenGT ((_, EQ) : xs) = go seenGT xs
    go _      ((_, GT) : xs) = go True   xs
{- ORMOLU_ENABLE -}

isSupportiveSubmapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSupportiveSubmapOf m1 m2 =
    Map.isSubmapOfBy (<=) (toMap m1) (toMap m2)

isProperSupportiveSubmapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSupportiveSubmapOf m1 m2 =
    Map.isProperSubmapOfBy (<=) (toMap m1) (toMap m2)

isSupportiveSupermapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSupportiveSupermapOf m1 m2 =
    Map.isSubmapOfBy (<=) (toMap m2) (toMap m1)

isProperSupportiveSupermapOf
    :: PackedCountMap p k c
    => Monoid (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSupportiveSupermapOf m1 m2 =
    Map.isProperSubmapOfBy (<=) (toMap m2) (toMap m1)

isSymmetricSubmapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSymmetricSubmapOf m1 m2 =
    Map.isSubmapOfBy isSymmetricallyBoundedBy (toMap m1) (toMap m2)

isProperSymmetricSubmapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSymmetricSubmapOf m1 m2 =
    Map.isProperSubmapOfBy isSymmetricallyBoundedBy (toMap m1) (toMap m2)

isSymmetricSupermapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isSymmetricSupermapOf m1 m2 =
    Map.isSubmapOfBy isSymmetricallyBoundedBy (toMap m2) (toMap m1)

isProperSymmetricSupermapOf
    :: PackedCountMap p k c
    => Group (Count c)
    => Ord c
    => Ord k
    => p -> p -> Bool
isProperSymmetricSupermapOf m1 m2 =
    Map.isProperSubmapOfBy isSymmetricallyBoundedBy (toMap m2) (toMap m1)

-- The set of all subsets.
powerset
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Semiring (Count c)
    => Ord c
    => Ord k
    => p -> Set p
powerset = Set.fromDistinctAscList . coerce . powersetElements

-- Generates all subsets in lexicographic order.
powersetElements
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => PositiveMonoid (Count c)
    => Semiring (Count c)
    => Ord c
    => Ord k
    => p -> [p]
{- ORMOLU_DISABLE -}
powersetElements =
    fmap (pack . MonoidMap.fromListWith (<>)) . go . MonoidMap.toList . unpack
  where
    go            [] = [[]]
    go ((a, p) : xs) = [(a, q) : ys | q <- semiringIntervalZero p, ys <- go xs]
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
    => Semiring (Count c)
    => Ord c
    => Ord k
    => p -> Set p
symmetricPowerset =
    Set.fromDistinctAscList . coerce . symmetricPowersetElements

-- Generates all symmetric subsets in lexicograhic order.
symmetricPowersetElements
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Group (Count c)
    => Semiring (Count c)
    => Ord c
    => Ord k
    => p -> [p]
{- ORMOLU_DISABLE -}
symmetricPowersetElements =
    fmap (pack . MonoidMap.fromListWith (<>)) . go . MonoidMap.toList . unpack
  where
    go            [] = [[]]
    go ((a, p) : xs) = [(a, q) : ys | q <- semiringIntervalZero p, ys <- go xs]
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

alignAsc
    :: forall p k c
     . PackedCountMap p k c
    => Ord k
    => Monoid (Count c)
    => p
    -> p
    -> [(k, (Count c, Count c))]
alignAsc p1 p2 =
    coerce $
        alignKeyValuePairs @k @(Count c)
            (coerce (toListAsc p1))
            (coerce (toListAsc p2))

alignDesc
    :: forall p k c
     . PackedCountMap p k c
    => Ord k
    => Monoid (Count c)
    => p
    -> p
    -> [(k, (Count c, Count c))]
alignDesc p1 p2 =
    coerce $
        alignKeyValuePairs @(Down k) @(Count c)
            (coerce (toListDesc p1))
            (coerce (toListDesc p2))

alignKeyValuePairs
    :: forall k v
     . Ord k
    => Monoid v
    => [(k, v)]
    -> [(k, v)]
    -> [(k, (v, v))]
{- ORMOLU_DISABLE -}
alignKeyValuePairs = go
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

isSymmetricallyBoundedBy :: (Ord a, Monoid (Count a)) => a -> a -> Bool
isSymmetricallyBoundedBy v1 v2
    | Count v1 <= mempty && Count v2 <= mempty = v1 >= v2
    | Count v1 >= mempty && Count v2 >= mempty = v1 <= v2
    | otherwise = False

semiringIntervalZero :: Ord a => Semiring a => a -> [a]
semiringIntervalZero = semiringInterval Semiring.zero

semiringInterval :: Ord a => Semiring a => a -> a -> [a]
semiringInterval a b = from lo
  where
    lo = Prelude.min a b
    hi = Prelude.max a b

    from n
        | n == hi = [n]
        | n < hi = n : from (semiringSucc n)
        | otherwise = error "semiringInterval"

semiringSucc :: Semiring a => a -> a
semiringSucc = Semiring.plus Semiring.one
