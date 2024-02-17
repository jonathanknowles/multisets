{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MultiSet
    ( MultiSet
    , MultiSetType (N, Z)
    , Multiplicity
    , IsMultiplicity
    , empty
    , fromList
    , maximum
    , minimum
    {-, cardinality
    , multiplicity

    , invert
    , intersection
    , union
    , emptyN
    , emptyZ
    , fromListN
    , fromListZ
    , toList-}
    , toMultiSetZ
    , toMultiSetN
    )
    where

import Prelude hiding
    ( gcd, maximum, minimum )

import Data.Coerce
    ( coerce )
import Data.Group
    ( Group )
import Data.MonoidMap
    ( MonoidMap )
import Data.Monoid.Null
    ( MonoidNull (null) )
import Numeric.Natural
    ( Natural )

import qualified Data.Foldable as F
import qualified Data.Group as Group
import qualified Data.MonoidMap as MonoidMap

newtype MultiSet (t :: MultiSetType) a =
    MultiSet {unwrap :: MonoidMap a (Sum (Multiplicity t))}

type MultiplicityConstraints m = (Eq m, Show m, Num m)

class MultiplicityConstraints m => IsMultiplicity m

deriving instance
    (IsMultiplicity (Multiplicity t), Eq a) => Eq (MultiSet t a)
deriving instance
    (IsMultiplicity (Multiplicity t), Show a) => Show (MultiSet t a)
deriving newtype instance
    (IsMultiplicity (Multiplicity t), Ord a) => Semigroup (MultiSet t a)
deriving newtype instance
    (IsMultiplicity (Multiplicity t), Ord a) => Monoid (MultiSet t a)
deriving newtype instance
    (IsMultiplicity (Multiplicity t), Ord a) => MonoidNull (MultiSet t a)

newtype Sum a = Sum {getSum :: a}
    deriving (Functor, Eq, Ord, Show)

instance (m ~ Multiplicity t, Num m) => Semigroup (Sum m) where
    Sum x <> Sum y = Sum (x + y)

instance (m ~ Multiplicity t, Num m) => Monoid (Sum m) where
    mempty = Sum 0

instance (m ~ Multiplicity t, Eq m, Num m) => MonoidNull (Sum m) where
    null (Sum 0) = True
    null _ = False

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
type family Multiplicity (t :: MultiSetType) = m | m -> t where
    Multiplicity N = Natural
    Multiplicity Z = Integer

empty :: MultiSet t a
empty = MultiSet MonoidMap.empty

fromList :: (IsMultiplicity (Multiplicity t), Ord a) => [(a, Multiplicity t)] -> MultiSet t a
fromList = MultiSet . MonoidMap.fromList . coerce


{-
emptyN :: MultiSetN a
emptyN = MultiSet MonoidMap.empty

emptyZ :: MultiSetZ a
emptyZ = MultiSet MonoidMap.empty

fromListN :: Ord a => [(a, Natural)] -> MultiSetN a
fromListN = MultiSet . MonoidMap.fromList . coerce

fromListZ :: Ord a => [(a, Integer)] -> MultiSetZ a
fromListZ = MultiSet . MonoidMap.fromList . coerce

toList :: MultiSet t a -> [(a, Multiplicity t)]
toList = coerce . MonoidMap.toList . unwrap
-}
toMultiSetZ :: Ord a => (MultiSetN a, MultiSetN a) -> MultiSetZ a
toMultiSetZ (MultiSet ns, MultiSet ps) = MultiSet $ (<>)
    (MonoidMap.map (fmap (negate . naturalToInteger)) ns)
    (MonoidMap.map (fmap (         naturalToInteger)) ps)

toMultiSetN :: MultiSetZ a -> (MultiSetN a, MultiSetN a)
toMultiSetN (MultiSet s) = (MultiSet ns, MultiSet ps)
  where
    ns = MonoidMap.map (fmap integerNegativePartToNatural) s
    ps = MonoidMap.map (fmap integerPositivePartToNatural) s
{-
cardinality :: MultiSet t a -> Multiplicity t
cardinality (MultiSet s) = getSum $ F.fold s

multiplicity :: Ord a => a -> MultiSet t a -> Multiplicity t
multiplicity a (MultiSet s) = getSum $ MonoidMap.get a s
-}
maximum :: forall a m t. (m ~ Multiplicity t, Num m, Ord m) => MultiSet t a -> Multiplicity t
maximum (MultiSet s) = if MonoidMap.null s then 0 else getSum $ F.maximum s

minimum :: forall a m t. (m ~ Multiplicity t, Num m, Ord m) => MultiSet t a -> Multiplicity t
minimum (MultiSet s) = if MonoidMap.null s then 0 else getSum $ F.minimum s
{-
invert :: MultiSet t a -> MultiSetZ a
invert (MultiSet s) =
    MultiSet (MonoidMap.map (fmap (negate . toInteger)) s)

intersection :: Ord a => MultiSet t a -> MultiSet t a -> MultiSet t a
intersection (MultiSet s1) (MultiSet s2) =
    MultiSet (MonoidMap.intersectionWith min s1 s2)

union :: Ord a => MultiSet t a -> MultiSet t a -> MultiSet t a
union (MultiSet s1) (MultiSet s2) =
    MultiSet (MonoidMap.unionWith max s1 s2)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
-}
naturalToInteger :: Natural -> Integer
naturalToInteger = fromIntegral

integerNegativePartToNatural :: Integer -> Natural
integerNegativePartToNatural n
    | n < 0 = fromIntegral (abs n)
    | otherwise = 0

integerPositivePartToNatural :: Integer -> Natural
integerPositivePartToNatural n
    | n > 0 = fromIntegral n
    | otherwise = 0
