{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Internal.Data.CountMap where

import Data.Coerce
    ( coerce
    )
import Data.Foldable1 (Foldable1)
import Data.Foldable1 qualified as Foldable1
import Data.Group
    ( Group
    )
import Data.Map.Strict
    ( Map
    )
import Data.Monoid.Null
    ( MonoidNull
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

unions1
    :: PackedCountMap p k c
    => MonoidNull (Count c)
    => Ord k
    => Ord c
    => Foldable1 f
    => f p -> p
unions1 = Foldable1.foldl1' union

showWith
    :: PackedCountMap p k c
    => Show k
    => Show c
    => String -> String -> p -> String
showWith typeName operatorName p =
    typeName <> ".fromListWith " <> operatorName <> " " <> show (toList p)

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

toMap :: forall p k c. PackedCountMap p k c => p -> Map k c
toMap = coerce @(Map k (Count c)) @(Map k c) . MonoidMap.toMap . unpack
