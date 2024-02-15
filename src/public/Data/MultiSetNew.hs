{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MultiSetNew
    ( MultiSet
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
    , empty
    , fromList
    , toList
    , toMultiSetN
    , toMultiSetZ
    ) where

import Prelude hiding
    ( gcd, maximum, minimum )

import Data.Coerce
    ( coerce )
import Data.Group
    ( Group )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Sum (..) )
import Data.MonoidMap
    ( MonoidMap )
import Numeric.Natural
    ( Natural )

import qualified Data.Foldable as F
import qualified Data.MonoidMap as MonoidMap

newtype MultiSet a m = MultiSet {unwrap :: MonoidMap a (Sum m)}
    deriving stock Eq

instance (Show a, Show m) => Show (MultiSet a m) where
    show
        = show
        . coerce @_ @(Map a m)
        . MonoidMap.toMap
        . unwrap

deriving newtype instance (Ord a, Multiplicity m) => Semigroup (MultiSet a m)
deriving newtype instance (Ord a, Multiplicity m) => Monoid (MultiSet a m)

deriving newtype instance Ord a => Group (MultiSetZ a)

-- | Represents a multiset with 'Natural' (ℕ) multiplicity.
type MultiSetN a = MultiSet a Natural

-- | Represents a multiset with 'Integer' (ℤ) multiplicity.
type MultiSetZ a = MultiSet a Integer

data MultiplicityS m where
    MultiplicityN :: MultiplicityS Natural
    MultiplicityZ :: MultiplicityS Integer

class (Num m, Ord m) => MultiplicityConstraints m where
    multiplicityS :: MultiplicityS m

instance MultiplicityConstraints Integer where
    multiplicityS = MultiplicityZ

instance MultiplicityConstraints Natural where
    multiplicityS = MultiplicityN

class MultiplicityConstraints m => Multiplicity m

instance Multiplicity Integer
instance Multiplicity Natural

empty :: forall a m. (Ord a, Multiplicity m) => MultiSet a m
empty = MultiSet mempty

fromList :: forall a m. (Ord a, Multiplicity m) => [(a, m)] -> MultiSet a m
fromList
    = MultiSet
    . MonoidMap.fromList
    . coerce @_ @[(a, Sum m)]

toList :: forall a m. MultiSet a m -> [(a, m)]
toList = coerce @_ @[(a, m)] . MonoidMap.toList . unwrap

toMultiSetZ :: MultiSetZ a -> (MultiSetN a, MultiSetN a)
toMultiSetZ (MultiSet s) =
    (MultiSet ns, MultiSet ps)
  where
    ns = MonoidMap.map (fmap integerNegativePartToNatural) s
    ps = MonoidMap.map (fmap integerPositivePartToNatural) s

toMultiSetN :: Ord a => (MultiSetN a, MultiSetN a) -> MultiSetZ a
toMultiSetN (MultiSet ns, MultiSet ps) =
    MultiSet $ (<>)
        (MonoidMap.map (fmap (negate . naturalToInteger)) ns)
        (MonoidMap.map (fmap (         naturalToInteger)) ps)

cardinality :: forall a m. Multiplicity m => MultiSet a m -> m
cardinality = getSum . F.fold . unwrap

multiplicity :: forall a m. (Ord a, Multiplicity m) => a -> MultiSet a m -> m
multiplicity a = getSum . MonoidMap.get a . unwrap

maximum :: Multiplicity m => MultiSet a m -> m
maximum (MultiSet s)
    | MonoidMap.null s = 0
    | otherwise = getSum $ F.maximum s

minimum :: Multiplicity m => MultiSet a m -> m
minimum (MultiSet s)
    | MonoidMap.null s = 0
    | otherwise = getSum $ F.minimum s

invert :: forall a m. Multiplicity m => MultiSet a m -> MultiSetZ a
invert (MultiSet s) = case multiplicityS @m of
    MultiplicityN -> MultiSet
        (MonoidMap.map (fmap (negate . naturalToInteger)) s)
    MultiplicityZ -> MultiSet
        (MonoidMap.map (fmap negate) s)

intersection
    :: (Ord a, Multiplicity m)
    => MultiSet a m
    -> MultiSet a m
    -> MultiSet a m
intersection (MultiSet s1) (MultiSet s2) =
    MultiSet (MonoidMap.intersectionWith min s1 s2)

union
    :: (Ord a, Multiplicity m)
    => MultiSet a m
    -> MultiSet a m
    -> MultiSet a m
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
