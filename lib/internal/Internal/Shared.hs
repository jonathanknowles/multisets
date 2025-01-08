module Internal.Shared where

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
import GHC.IsList
    ( IsList (..)
    )
import Internal.Data.CountMap
    ( CountMap
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Data.Packed
    ( Packed (Unpacked)
    )
import Internal.Data.Sign
    ( Sign
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Bag a = Bag (CountMap a Natural)
    deriving newtype (Eq)
    deriving newtype (Semigroup, Monoid, MonoidNull, PositiveMonoid)
    deriving newtype (Commutative, OverlappingGCDMonoid, Monus)
    deriving newtype (Cancellative, LeftCancellative, RightCancellative)
    deriving newtype (Reductive, LeftReductive, RightReductive)

newtype SignedBag a = SignedBag (CountMap a Integer)
    deriving newtype (Eq)
    deriving newtype (Semigroup, Monoid, MonoidNull)
    deriving newtype (Commutative, Group)

newtype SignedSet a = SignedSet (CountMap a Sign)
    deriving newtype (Eq)
    deriving newtype (Semigroup, Monoid, MonoidNull)
    deriving newtype (Commutative, Group)

instance Packed (Bag a) where
    type Unpacked (Bag a) = CountMap a Natural

instance Packed (SignedBag a) where
    type Unpacked (SignedBag a) = CountMap a Integer

instance Packed (SignedSet a) where
    type Unpacked (SignedSet a) = CountMap a Sign

instance Ord a => IsList (Bag a) where
    type Item (Bag a) = (a, Natural)
    fromList = CountMap.fromList
    toList = CountMap.toList

instance Ord a => IsList (SignedBag a) where
    type Item (SignedBag a) = (a, Integer)
    fromList = CountMap.fromList
    toList = CountMap.toList

instance Ord a => IsList (SignedSet a) where
    type Item (SignedSet a) = (a, Sign)
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
{-

{- ORMOLU_DISABLE -}
instance Foldable Bag where
    fold     = CountMap.foldRoots
    foldMap  = CountMap.foldMapRoots
    foldMap' = CountMap.foldMapRoots'
    foldr    = CountMap.foldrRoots
    foldr'   = CountMap.foldrRoots'
    foldl    = CountMap.foldlRoots
    foldl'   = CountMap.foldlRoots'
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
instance Foldable SignedBag where
    fold     = CountMap.foldRoots
    foldMap  = CountMap.foldMapRoots
    foldMap' = CountMap.foldMapRoots'
    foldr    = CountMap.foldrRoots
    foldr'   = CountMap.foldrRoots'
    foldl    = CountMap.foldlRoots
    foldl'   = CountMap.foldlRoots'
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
instance Foldable SignedSet where
    fold     = CountMap.foldRoots
    foldMap  = CountMap.foldMapRoots
    foldMap' = CountMap.foldMapRoots'
    foldr    = CountMap.foldrRoots
    foldr'   = CountMap.foldrRoots'
    foldl    = CountMap.foldlRoots
    foldl'   = CountMap.foldlRoots'
{- ORMOLU_ENABLE -}
-}
