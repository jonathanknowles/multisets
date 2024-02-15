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
    , Multiplicity
    , empty
    , fromList
    , toList
    , mergeSigned
    , splitSigned
    , cardinality
    , multiplicity
    , invert
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
import qualified Data.Group as Group
import qualified Data.MonoidMap as MonoidMap

data MultiplicityS m where
    MultiplicityN :: MultiplicityS Natural
    MultiplicityZ :: MultiplicityS Integer

class (Eq m, Num m) => Multiplicity m where
    multiplicityS :: MultiplicityS m

instance Multiplicity Integer where
    multiplicityS = MultiplicityZ

instance Multiplicity Natural where
    multiplicityS = MultiplicityN

newtype MultiSet a m = UnsafeMultiSet {unwrap :: MonoidMap a (Sum m)}
    deriving stock Eq

type MultiSetN a = MultiSet a Natural
type MultiSetZ a = MultiSet a Integer

instance (Show a, Show m) => Show (MultiSet a m) where
    show = show . coerce @_ @(Map a m) . MonoidMap.toMap . unwrap

withMultiplicity :: MultiplicityS m -> (Multiplicity m => r) -> r
withMultiplicity m r = case m of
    MultiplicityN -> r
    MultiplicityZ -> r

empty :: forall a m. (Ord a, Multiplicity m) => MultiSet a m
empty = withMultiplicity (multiplicityS @m) (UnsafeMultiSet mempty)

fromList :: forall a m. (Ord a, Multiplicity m) => [(a, m)] -> MultiSet a m
fromList
    = withMultiplicity (multiplicityS @m)
    . UnsafeMultiSet
    . MonoidMap.fromList
    . coerce @_ @[(a, Sum m)]

toList :: forall a m. MultiSet a m -> [(a, m)]
toList = coerce @_ @[(a, m)] . MonoidMap.toList . unwrap

mergeSigned
    :: Ord a
    => MultiSet a Natural
    -> MultiSet a Natural
    -> MultiSet a Integer
mergeSigned (UnsafeMultiSet ns) (UnsafeMultiSet ps) =
    UnsafeMultiSet $ (<>)
        (MonoidMap.map (fmap (negate . naturalToInteger)) ns)
        (MonoidMap.map (fmap (         naturalToInteger)) ps)

splitSigned :: MultiSet a Integer -> (MultiSet a Natural, MultiSet a Natural)
splitSigned (UnsafeMultiSet s) =
    (UnsafeMultiSet ns, UnsafeMultiSet ps)
  where
    ns = MonoidMap.map (fmap integerNegativePartToNatural) s
    ps = MonoidMap.map (fmap integerPositivePartToNatural) s

cardinality :: forall a m. Multiplicity m => MultiSet a m -> m
cardinality
    = withMultiplicity (multiplicityS @m)
    $ getSum . F.fold . unwrap

multiplicity :: forall a m. (Ord a, Multiplicity m) => a -> MultiSet a m -> m
multiplicity
    = withMultiplicity (multiplicityS @m)
    $ \a -> getSum . MonoidMap.get a . unwrap

invert :: forall a m. Multiplicity m => MultiSet a m -> MultiSet a Integer
invert (UnsafeMultiSet s) = case multiplicityS @m of
    MultiplicityN -> UnsafeMultiSet
        (MonoidMap.map (fmap (negate . naturalToInteger)) s)
    MultiplicityZ -> UnsafeMultiSet
        (MonoidMap.map (fmap negate) s)

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
