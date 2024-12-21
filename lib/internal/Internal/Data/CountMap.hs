{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Internal.Data.CountMap where

import Data.Coerce
    ( coerce
    )
import Data.Foldable qualified as Foldable
import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Data.Function (on)
import Data.Group
    ( Group
    )
import Data.Map.Strict
    ( Map
    )
import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Internal.Data.Packed
    ( Packed (Unpacked, pack, unpack)
    , unpacked
    , unpacked2
    )
import Prelude

newtype Count a = Count a
    deriving stock (Eq, Ord, Functor)

instance Packed (Count a) where
    type Unpacked (Count a) = a

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

fromListWith
    :: PackedCountMap p k c
    => Ord k
    => MonoidNull (Count c)
    => (c -> c -> c)
    -> [(k, c)]
    -> p
fromListWith f xs = pack $ MonoidMap.fromListWith (coerce f) (coerce xs)

toList :: forall p k c. PackedCountMap p k c => p -> [(k, c)]
toList = coerce @([(k, Count c)]) @([(k, c)]) . MonoidMap.toList . unpack

fromMap
    :: forall p k c
     . PackedCountMap p k c
    => MonoidNull (Count c)
    => Map k c -> p
fromMap = pack . MonoidMap.fromMap . coerce @(Map k c) @(Map k (Count c))

toMap :: forall p k c. PackedCountMap p k c => p -> Map k c
toMap = coerce @(Map k (Count c)) @(Map k c) . MonoidMap.toMap . unpack

lookup
    :: PackedCountMap p k c
    => Ord k
    => Monoid (Count c)
    => k -> p -> c
lookup a = unpack . MonoidMap.get a . unpack

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

-- Note: evaluation will terminate early if (and only if) the maps are
-- incomparable.
--
{- ORMOLU_DISABLE -}
compare
    :: PackedCountMap p k c
    => Monoid c
    => Ord c
    => Ord k
    => p -> p -> Maybe Ordering
compare s1 s2 = go False False (compareElements s1 s2)
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
    => Monoid c
    => Ord c
    => Ord k
    => p -> p -> [(k, Ordering)]
compareElements s1 s2 = fmap (uncurry Prelude.compare) <$> align s1 s2

isLessThanOrEqualTo
    :: PackedCountMap p k c
    => Monoid c
    => Ord c
    => Ord k
    => p -> p -> Bool
isLessThanOrEqualTo m1 m2 = GT `notElem` (snd <$> compareElements m1 m2)

isGreaterThanOrEqualTo
    :: PackedCountMap p k c
    => Monoid c
    => Ord c
    => Ord k
    => p -> p -> Bool
isGreaterThanOrEqualTo s1 s2 = LT `notElem` (snd <$> compareElements s1 s2)

-- Note: evaluation will terminate early if (and only if) a GT is detected.
--
{- ORMOLU_DISABLE -}
isLessThan
    :: PackedCountMap p k c
    => Monoid c
    => Ord c
    => Ord k
    => p -> p -> Bool
isLessThan m1 m2 = go False (compareElements m1 m2)
  where
    go seenLT             [] = seenLT
    go _      ((_, LT) : xs) = go True   xs
    go seenLT ((_, EQ) : xs) = go seenLT xs
    go _      ((_, GT) :  _) = False
{- ORMOLU_ENABLE -}

-- Note: evaluation will terminate early if (and only if) a LT is detected.
--
{- ORMOLU_DISABLE -}
isGreaterThan
    :: PackedCountMap p k c
    => Monoid c
    => Ord c
    => Ord k
    => p -> p -> Bool
isGreaterThan m1 m2 = go False (compareElements m1 m2)
  where
    go seenGT             [] = seenGT
    go _      ((_, LT) :  _) = False
    go seenGT ((_, EQ) : xs) = go seenGT xs
    go _      ((_, GT) : xs) = go True   xs
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
align
    :: PackedCountMap p k c
    => Ord k
    => Monoid c
    => p
    -> p
    -> [(k, (c, c))]
align = go `on` toList
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
