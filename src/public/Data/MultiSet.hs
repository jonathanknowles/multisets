{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

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

import Data.Coerce
    ( coerce )
import Data.Group
    ( Group )
import Data.Monoid
    ( Sum (..) )
import Data.MonoidMap
    ( MonoidMap )
import Numeric.Natural
    ( Natural )

import qualified Data.Foldable as F
import qualified Data.Group as Group
import qualified Data.MonoidMap as MonoidMap

data MultiSet (t :: MultiSetType) a =
    MultiplicityConstraints (Multiplicity t) =>
    MultiSet {unwrap :: MonoidMap a (Sum (Multiplicity t))}

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

type MultiplicityConstraints t = (Eq t, Integral t, Num t, Ord t, Show t)

deriving instance Eq a => Eq (MultiSet t a)
deriving instance Show a => Show (MultiSet t a)

instance Ord a => Semigroup (MultiSet t a) where
    MultiSet s1 <> MultiSet s2 = MultiSet (s1 <> s2)

instance Ord a => Monoid (MultiSetN a) where
    mempty = MultiSet mempty
instance Ord a => Monoid (MultiSetZ a) where
    mempty = MultiSet mempty

instance Ord a => Group (MultiSetZ a) where
    invert (MultiSet s) = MultiSet (MonoidMap.invert s)

emptyN :: MultiSetN a
emptyN = MultiSet MonoidMap.empty

emptyZ :: MultiSetZ a
emptyZ = MultiSet MonoidMap.empty

fromListN :: Ord a => [(a, Natural)] -> MultiSetN a
fromListN = MultiSet . MonoidMap.fromList . coerce

fromListZ :: Ord a => [(a, Integer)] -> MultiSetZ a
fromListZ = MultiSet . MonoidMap.fromList . coerce

toList :: MultiSet t a -> [(a, Multiplicity t)]
toList (MultiSet s) = fmap getSum <$> MonoidMap.toList s

toMultiSetZ :: Ord a => (MultiSetN a, MultiSetN a) -> MultiSetZ a
toMultiSetZ (MultiSet ns, MultiSet ps) = MultiSet $ (<>)
    (MonoidMap.map (fmap (negate . naturalToInteger)) ns)
    (MonoidMap.map (fmap (         naturalToInteger)) ps)

toMultiSetN :: MultiSetZ a -> (MultiSetN a, MultiSetN a)
toMultiSetN (MultiSet s) = (MultiSet ns, MultiSet ps)
  where
    ns = MonoidMap.map (fmap integerNegativePartToNatural) s
    ps = MonoidMap.map (fmap integerPositivePartToNatural) s

cardinality :: MultiSet t a -> Multiplicity t
cardinality (MultiSet s) = getSum $ F.fold s

multiplicity :: Ord a => a -> MultiSet t a -> Multiplicity t
multiplicity a (MultiSet s) = getSum $ MonoidMap.get a s

maximum :: MultiSet t a -> Multiplicity t
maximum (MultiSet s) = if MonoidMap.null s then 0 else getSum $ F.maximum s

minimum :: MultiSet t a -> Multiplicity t
minimum (MultiSet s) = if MonoidMap.null s then 0 else getSum $ F.minimum s

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
