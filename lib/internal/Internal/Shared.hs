{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Shared where

import Control.DeepSeq
    ( NFData
    )
import Data.Group
    ( Group
    )
import Data.Monoid.Monus
    ( Monus
    , OverlappingGCDMonoid
    )
import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Semigroup.Cancellative
    ( Cancellative
    , LeftCancellative
    , LeftReductive
    , Reductive
    , RightCancellative
    , RightReductive
    )
import Data.Semigroup.Commutative
    ( Commutative
    )
import Data.Set
    ( Set
    )
import GHC.IsList
    ( IsList (..)
    )
import Internal.Data.Count
    ( Count (Count)
    )
import Internal.Data.CountMap
    ( CountMap
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Data.Packed
    ( Packed (Unpacked, pack, unpack)
    )
import Internal.Data.Sign
    ( NumSign
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Bag a = Bag (CountMap a Natural)
    deriving newtype (Eq, NFData)
    deriving newtype (Semigroup, Monoid, MonoidNull, PositiveMonoid)
    deriving newtype (Commutative, OverlappingGCDMonoid, Monus)
    deriving newtype (Cancellative, LeftCancellative, RightCancellative)
    deriving newtype (Reductive, LeftReductive, RightReductive)

newtype SignedBag a = SignedBag (CountMap a Integer)
    deriving newtype (Eq, NFData)
    deriving newtype (Semigroup, Monoid, MonoidNull)
    deriving newtype (Commutative, Group)

newtype SignedSet a = SignedSet (CountMap a NumSign)
    deriving newtype (Eq, NFData)
    deriving newtype (Semigroup, Monoid, MonoidNull)
    deriving newtype (Commutative, Group)

-- | See 'Data.Bag.compareLexically'.
instance Ord a => Ord (Bag a) where
    compare = CountMap.compareLexically

-- | See 'Data.Bag.Signed.compareLexically'.
instance Ord a => Ord (SignedBag a) where
    compare = CountMap.compareLexically

-- | See 'Data.Set.Signed.compareLexically'.
instance Ord a => Ord (SignedSet a) where
    compare = CountMap.compareLexically

instance Packed (Bag a) where
    type Unpacked (Bag a) = CountMap a Natural

instance Packed (SignedBag a) where
    type Unpacked (SignedBag a) = CountMap a Integer

instance Packed (SignedSet a) where
    type Unpacked (SignedSet a) = CountMap a NumSign

-- TODO:
--
-- This instance allows us to treat a @Set a@ object as if it were a packed
-- 'CountMap a Bool' object. However, packing and unpacking both have cost
-- that increases (superlinearly) with the size of the set.

-- We should think of a way to avoid defining this instance.
--
instance Packed (Set a) where
    type Unpacked (Set a) = CountMap a Bool
    unpack = MonoidMap.fromSet (const (Count True))
    pack = MonoidMap.nonNullKeys

instance Ord a => IsList (Bag a) where
    type Item (Bag a) = (a, Natural)
    fromList = CountMap.fromList
    toList = CountMap.toList

instance Ord a => IsList (SignedBag a) where
    type Item (SignedBag a) = (a, Integer)
    fromList = CountMap.fromList
    toList = CountMap.toList

instance Ord a => IsList (SignedSet a) where
    type Item (SignedSet a) = (a, NumSign)
    fromList = CountMap.fromList
    toList = CountMap.toList

instance Show a => Show (Bag a) where
    show = CountMap.showFromList "Bag"

instance Show a => Show (SignedBag a) where
    show = CountMap.showFromList "SignedBag"

instance Show a => Show (SignedSet a) where
    show = CountMap.showFromListWith "SignedSet" "Sign.add"

-- Think about the relative utility of:
--
-- - folding over just the roots of a set
-- - folding over each element n times, where n is the multiplicity
--
-- Consider whether the latter can be made more efficient in the case of large
-- multiplicities.
--
-- There are many possible cases to consider:
--
-- Roots         - fold over the entire root set
-- RootsNegative - fold over the negative subset of the root set (not for Bag)
-- RootsPositive - fold over the positive subset of the root set (not for Bag)
-- Unary         - fold over all repetitions of each element (only for Bag)
-- UnaryNegative - fold over all repetitions of each positive element
-- UnaryPositive - fold over all repetitions of each negative element
