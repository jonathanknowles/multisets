{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MultiSet
    ( MultiSet
    , MultiSetType (..)
    , MultiSetN
    , MultiSetZ
    , Multiplicity
    , cardinality
    , multiplicity
    , maximum
    , minimum
    , invert
    , intersection
    , union
    , emptyN
    , emptyZ
    , fromListN
    , fromListZ
    , toList
    , toMultiSetZ
    , toMultiSetN
    )
    where

import Prelude hiding
    ( gcd, maximum, minimum )

import Data.Group
    ( Group )
import Data.Monoid
    ( Sum (..) )
import Data.Total.MonoidMap
    ( MonoidMap )
import Numeric.Natural
    ( Natural )

import qualified Data.Foldable as F
import qualified Data.Group as Group
import qualified Data.Total.MonoidMap as MonoidMap

data MultiSet (t :: MultiSetType) a where
    MultiSetN :: MonoidMap a (Sum Natural) -> MultiSetN a
    MultiSetZ :: MonoidMap a (Sum Integer) -> MultiSetZ a

data MultiSetType
    -- | Indicates a multiset with 'Natural' (ℕ) multiplicity.
    = N
    -- | Indicates a multiset with 'Integer' (ℤ) multiplicity.
    | Z

-- | Represents a multiset with 'Natural' (ℕ) multiplicity.
type MultiSetN = MultiSet N

-- | Represents a multiset with 'Integer' (ℤ) multiplicity.
type MultiSetZ = MultiSet Z

-- | Maps the type of a multiset to the type of its multiplicity.
type family Multiplicity (t :: MultiSetType) where
    Multiplicity N = Natural
    Multiplicity Z = Integer

deriving instance Eq a => Eq (MultiSet t a)
deriving instance Show a => Show (MultiSet t a)

instance Ord a => Semigroup (MultiSet t a) where
    MultiSetN s1 <> MultiSetN s2 = MultiSetN (s1 <> s2)
    MultiSetZ s1 <> MultiSetZ s2 = MultiSetZ (s1 <> s2)

instance Ord a => Monoid (MultiSetN a) where
    mempty = MultiSetN mempty
instance Ord a => Monoid (MultiSetZ a) where
    mempty = MultiSetZ mempty

instance Ord a => Group (MultiSetZ a) where
    invert (MultiSetZ s) = MultiSetZ (MonoidMap.invert s)

emptyN :: MultiSetN a
emptyN = MultiSetN MonoidMap.empty

emptyZ :: MultiSetZ a
emptyZ = MultiSetZ MonoidMap.empty

fromListN :: Ord a => [(a, Natural)] -> MultiSetN a
fromListN kvs = MultiSetN (MonoidMap.fromList (fmap Sum <$> kvs))

fromListZ :: Ord a => [(a, Integer)] -> MultiSetZ a
fromListZ kvs = MultiSetZ (MonoidMap.fromList (fmap Sum <$> kvs))

toList :: MultiSet t a -> [(a, Multiplicity t)]
toList = \case
    MultiSetN s -> fmap getSum <$> MonoidMap.toList s
    MultiSetZ s -> fmap getSum <$> MonoidMap.toList s

toMultiSetZ :: MultiSetN a -> MultiSetZ a
toMultiSetZ (MultiSetN s) = MultiSetZ $ MonoidMap.map (fmap fromIntegral) s

toMultiSetN :: MultiSetZ a -> (MultiSetN a, MultiSetN a)
toMultiSetN (MultiSetZ s) = (MultiSetN ps, MultiSetN ns)
  where
    ps = MonoidMap.map (fmap  fromIntegral       ) (MonoidMap.filter (> 0) s)
    ns = MonoidMap.map (fmap (fromIntegral . abs)) (MonoidMap.filter (< 0) s)

cardinality :: MultiSet t a -> Multiplicity t
cardinality = \case
    MultiSetN s -> getSum $ F.fold s
    MultiSetZ s -> getSum $ F.fold s

multiplicity :: Ord a => a -> MultiSet t a -> Multiplicity t
multiplicity a = \case
    MultiSetN s -> getSum $ MonoidMap.get a s
    MultiSetZ s -> getSum $ MonoidMap.get a s

maximum :: MultiSet t a -> Multiplicity t
maximum = \case
    MultiSetN s -> getSum $ F.maximum s
    MultiSetZ s -> getSum $ F.maximum s

minimum :: MultiSet t a -> Multiplicity t
minimum = \case
    MultiSetN s -> getSum $ F.minimum s
    MultiSetZ s -> getSum $ F.minimum s

invert :: MultiSet t a -> MultiSetZ a
invert = \case
    MultiSetN s -> MultiSetZ
        (MonoidMap.map (fmap (negate . fromIntegral)) s)
    MultiSetZ s -> MultiSetZ
        (MonoidMap.map (fmap negate) s)

intersection :: Ord a => MultiSet t a -> MultiSet t a -> MultiSet t a
intersection (MultiSetN s1) (MultiSetN s2) =
    MultiSetN (MonoidMap.intersection min s1 s2)
intersection (MultiSetZ s1) (MultiSetZ s2) =
    MultiSetZ (MonoidMap.intersection min s1 s2)

union :: Ord a => MultiSet t a -> MultiSet t a -> MultiSet t a
union (MultiSetN s1) (MultiSetN s2) =
    MultiSetN (MonoidMap.union max s1 s2)
union (MultiSetZ s1) (MultiSetZ s2) =
    MultiSetZ (MonoidMap.union max s1 s2)
