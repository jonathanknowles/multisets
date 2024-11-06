{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Multiset.Signed where

import Prelude hiding
    ( sum )

import Data.Coerce
    ( coerce )
import Data.MonoidMap
    ( MonoidMap )
import Data.Multiset.Combinators
    ( Sum (..), Union (..), Intersection (..) )

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Monoid
    ( Sum (Sum) )
import Data.Multiset (Multiset)
import qualified Data.Multiset as Multiset
import Numeric.Natural (Natural)
import Data.List (partition)

newtype SignedMultiset a = SignedMultiset
    (MonoidMap a (Data.Monoid.Sum Integer))

instance Show a => Show (SignedMultiset a) where
    show s = "fromListWith (+) " <> show (toList s)

instance Ord a => Semigroup (Sum (SignedMultiset a)) where
    (<>) = coerce sum
instance Ord a => Monoid (Sum (SignedMultiset a)) where
    mempty = coerce empty

instance Ord a => Semigroup (Union (SignedMultiset a)) where
    (<>) = coerce union
instance Ord a => Monoid (Union (SignedMultiset a)) where
    mempty = coerce empty

instance Ord a => Semigroup (Intersection (SignedMultiset a)) where
    (<>) = coerce intersection
instance Ord a => Monoid (Intersection (SignedMultiset a)) where
    mempty = coerce empty

fromListWith
    :: Ord a
    => (Integer -> Integer -> Integer)
    -> [(a, Integer)]
    -> SignedMultiset a
fromListWith f
    = SignedMultiset
    . MonoidMap.fromListWith (coerce f)
    . fmap (fmap Data.Monoid.Sum)

empty :: SignedMultiset a
empty = SignedMultiset MonoidMap.empty

toList :: SignedMultiset a -> [(a, Integer)]
toList (SignedMultiset s) = coerce (MonoidMap.toList s)

toUnsignedPair :: Ord a => SignedMultiset a -> (Multiset a, Multiset a)
toUnsignedPair m =
    ( Multiset.fromListWith (+) $
        fmap (fmap (fromIntegral @Integer @Natural . abs))
        ns
    , Multiset.fromListWith (+) $
        fmap (fmap (fromIntegral @Integer @Natural))
        ps
    )
  where
    (ns, ps) = partition ((< 0) . snd) (toList m)

fromUnsignedPairWith
    :: forall a. Ord a
    => (Integer -> Integer -> Integer)
    -> (Multiset a, Multiset a)
    -> SignedMultiset a
fromUnsignedPairWith f (s1, s2) =
    fromListWith f (ns <> ps)
  where
    ns :: [(a, Integer)]
    ns = fmap (negate . fromIntegral @Natural @Integer) <$>
        Multiset.toList s1
    ps :: [(a, Integer)]
    ps = fmap (fromIntegral @Natural @Integer) <$>
        Multiset.toList s2

difference
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
difference (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ m1 `MonoidMap.minus` m2

sum
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
sum (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.unionWith (+) m1 m2

union
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
union (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.unionWith max m1 m2

intersection
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
intersection (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.intersectionWith min m1 m2
