module Data.Multiset where

import Prelude hiding
    ( sum )

import Data.MonoidMap
    ( MonoidMap )
import Numeric.Natural
    ( Natural )
import Data.Coerce
    ( coerce )
import Data.Multiset.Combinators
    ( Sum (..), Union (..) )

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Monoid
    ( Sum (Sum) )

newtype Multiset a = Multiset (MonoidMap a (Data.Monoid.Sum Natural))

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

fromListWith
    :: Ord a
    => (Natural -> Natural -> Natural)
    -> [(a, Natural)]
    -> Multiset a
fromListWith f
    = Multiset
    . MonoidMap.fromListWith (coerce f)
    . coerce

toList :: Multiset a -> [(a, Natural)]
toList (Multiset s) = coerce (MonoidMap.toList s)

empty :: Multiset a
empty = Multiset MonoidMap.empty

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (Multiset m1) (Multiset m2) =
    Multiset $ m1 `MonoidMap.monus` m2

sum :: Ord a => Multiset a -> Multiset a -> Multiset a
sum (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.unionWith (+) m1 m2

union :: Ord a => Multiset a -> Multiset a -> Multiset a
union (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.unionWith max m1 m2

intersection :: Ord a => Multiset a -> Multiset a -> Multiset a
intersection (Multiset m1) (Multiset m2) =
    Multiset $ MonoidMap.intersectionWith min m1 m2
