{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Shared
    ( Bag (Bag)
    , SignedBag (SignedBag)
    , SignedSet (SignedSet)
    )
where

import Control.DeepSeq
    ( NFData
    )
import Data.Coerce
    ( coerce
    )
import Data.Group
    ( Group
    )
import Data.Monoid.GCD
    ( DistributiveGCDMonoid
    , GCDMonoid
    , LeftDistributiveGCDMonoid
    , LeftGCDMonoid
    , OverlappingGCDMonoid
    , RightDistributiveGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.LCM
    ( DistributiveLCMMonoid
    , LCMMonoid
    )
import Data.Monoid.Monus
    ( Monus
    )
import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.Monoid.Null qualified
import Data.MonoidMap ()
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
import Internal.Data.Semigroup.Transformers
    ( Intersection (..)
    , Product (..)
    , Sum (..)
    , Union (..)
    )
import Internal.Data.Sign.Num
    ( NumSign
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Bag a = Bag (CountMap a Natural)
    deriving newtype (Eq, NFData)

newtype SignedBag a = SignedBag (CountMap a Integer)
    deriving newtype (Eq, NFData)

newtype SignedSet a = SignedSet (CountMap a NumSign)
    deriving newtype (Eq, NFData)

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Instances for 'Sum' of 'Bag'
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
deriving via UBag a instance Ord a => Cancellative (Sum (Bag a))
deriving via UBag a instance Ord a => Commutative (Sum (Bag a))
deriving via UBag a instance Ord a => DistributiveGCDMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => DistributiveLCMMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => GCDMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => LCMMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => LeftCancellative (Sum (Bag a))
deriving via UBag a instance Ord a => LeftDistributiveGCDMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => LeftGCDMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => LeftReductive (Sum (Bag a))
deriving via UBag a instance Ord a => Monoid (Sum (Bag a))
deriving via UBag a instance Ord a => MonoidNull (Sum (Bag a))
deriving via UBag a instance Ord a => Monus (Sum (Bag a))
deriving via UBag a instance Ord a => OverlappingGCDMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => PositiveMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => Reductive (Sum (Bag a))
deriving via UBag a instance Ord a => RightCancellative (Sum (Bag a))
deriving via UBag a instance Ord a => RightDistributiveGCDMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => RightGCDMonoid (Sum (Bag a))
deriving via UBag a instance Ord a => RightReductive (Sum (Bag a))
deriving via UBag a instance Ord a => Semigroup (Sum (Bag a))
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Instances for 'Sum' of 'SignedBag'
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
deriving via USBag a instance Ord a => Cancellative (Sum (SignedBag a))
deriving via USBag a instance Ord a => Commutative (Sum (SignedBag a))
deriving via USBag a instance Ord a => Group (Sum (SignedBag a))
deriving via USBag a instance Ord a => LeftCancellative (Sum (SignedBag a))
deriving via USBag a instance Ord a => LeftReductive (Sum (SignedBag a))
deriving via USBag a instance Ord a => Monoid (Sum (SignedBag a))
deriving via USBag a instance Ord a => MonoidNull (Sum (SignedBag a))
deriving via USBag a instance Ord a => Reductive (Sum (SignedBag a))
deriving via USBag a instance Ord a => RightCancellative (Sum (SignedBag a))
deriving via USBag a instance Ord a => RightReductive (Sum (SignedBag a))
deriving via USBag a instance Ord a => Semigroup (Sum (SignedBag a))
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Instances for 'Sum' of 'SignedSet'
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
deriving via USSet a instance Ord a => Commutative (Sum (SignedSet a))
deriving via USSet a instance Ord a => Group (Sum (SignedSet a))
deriving via USSet a instance Ord a => Monoid (Sum (SignedSet a))
deriving via USSet a instance Ord a => MonoidNull (Sum (SignedSet a))
deriving via USSet a instance Ord a => Semigroup (Sum (SignedSet a))
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Instances for 'Product' of 'Bag'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Product (Bag a))

instance Ord a => Semigroup (Product (Bag a)) where
    (<>) = coerce (CountMap.multiply @(Bag a))

--------------------------------------------------------------------------------
-- Instances for 'Product' of 'SignedBag'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Product (SignedBag a))

instance Ord a => Semigroup (Product (SignedBag a)) where
    (<>) = coerce (CountMap.multiply @(SignedBag a))

--------------------------------------------------------------------------------
-- Instances for 'Product' of 'SignedSet'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Product (SignedSet a))

instance Ord a => Semigroup (Product (SignedSet a)) where
    (<>) = coerce (CountMap.multiply @(SignedSet a))

--------------------------------------------------------------------------------
-- Instances for 'Union' of 'Bag'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Union (Bag a))

instance Ord a => Monoid (Union (Bag a)) where
    mempty = coerce (CountMap.empty @(Bag a))

instance Ord a => MonoidNull (Union (Bag a)) where
    null = coerce (CountMap.null @(Bag a))

instance Ord a => PositiveMonoid (Union (Bag a))

instance Ord a => Semigroup (Union (Bag a)) where
    (<>) = coerce (CountMap.union @(Bag a))

--------------------------------------------------------------------------------
-- Instances for 'Union' of 'SignedBag'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Union (SignedBag a))

instance Ord a => Semigroup (Union (SignedBag a)) where
    (<>) = coerce (CountMap.union @(SignedBag a))

--------------------------------------------------------------------------------
-- Instances for 'Union' of 'SignedSet'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Union (SignedSet a))

instance Ord a => Semigroup (Union (SignedSet a)) where
    (<>) = coerce (CountMap.union @(SignedSet a))

--------------------------------------------------------------------------------
-- Instances for 'Intersection' of 'Bag'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Intersection (Bag a))

instance Ord a => Semigroup (Intersection (Bag a)) where
    (<>) = coerce (CountMap.intersection @(Bag a))

--------------------------------------------------------------------------------
-- Instances for 'Intersection' of 'SignedBag'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Intersection (SignedBag a))

instance Ord a => Semigroup (Intersection (SignedBag a)) where
    (<>) = coerce (CountMap.intersection @(SignedBag a))

--------------------------------------------------------------------------------
-- Instances for 'Intersection' of 'SignedSet'
--------------------------------------------------------------------------------

instance Ord a => Commutative (Intersection (SignedSet a))

instance Ord a => Semigroup (Intersection (SignedSet a)) where
    (<>) = coerce (CountMap.intersection @(SignedSet a))

--------------------------------------------------------------------------------
-- Instances of 'Ord'
--------------------------------------------------------------------------------

-- | See 'Data.Bag.compareLexically'.
instance Ord a => Ord (Bag a) where
    compare = CountMap.compareLexically

-- | See 'Data.Bag.Signed.compareLexically'.
instance Ord a => Ord (SignedBag a) where
    compare = CountMap.compareLexically

-- | See 'Data.Set.Signed.compareLexically'.
instance Ord a => Ord (SignedSet a) where
    compare = CountMap.compareLexically

--------------------------------------------------------------------------------
-- Instances of 'Packed'
--------------------------------------------------------------------------------

instance Packed (Bag a) where
    type Unpacked (Bag a) = CountMap a Natural

instance Packed (SignedBag a) where
    type Unpacked (SignedBag a) = CountMap a Integer

instance Packed (SignedSet a) where
    type Unpacked (SignedSet a) = CountMap a NumSign

type UBag a = Unpacked (Bag a)

type USBag a = Unpacked (SignedBag a)

type USSet a = Unpacked (SignedSet a)

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

--------------------------------------------------------------------------------
-- Instances of 'IsList'
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Instances of 'Show'
--------------------------------------------------------------------------------

instance Show a => Show (Bag a) where
    show = CountMap.showFromList "Bag"

instance Show a => Show (SignedBag a) where
    show = CountMap.showFromList "SignedBag"

instance Show a => Show (SignedSet a) where
    show = CountMap.showFromListWith "SignedSet" "Sign.add"
