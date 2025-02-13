module Internal.Data.Set.Transformers
    ( Sum (..)
    , Product (..)
    , Union (..)
    , Intersection (..)
    )
where

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
import Data.Semigroup.Cancellative
    ( LeftReductive
    , Reductive
    , RightReductive
    )
import Data.Semigroup.Commutative
    ( Commutative
    )
import Data.Set
    ( Set
    )
import Prelude

newtype Sum a = Sum {getSum :: a}
    deriving stock (Eq, Show)

newtype Product a = Product {getProduct :: a}
    deriving stock (Eq, Show)

newtype Union a = Union {getUnion :: a}
    deriving stock (Eq, Show)

newtype Intersection a = Intersection {getIntersection :: a}
    deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Instances for 'Union' of 'Set'
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
deriving newtype instance Ord a => Commutative (Union (Set a))
deriving newtype instance Ord a => DistributiveGCDMonoid (Union (Set a))
deriving newtype instance Ord a => DistributiveLCMMonoid (Union (Set a))
deriving newtype instance Ord a => GCDMonoid (Union (Set a))
deriving newtype instance Ord a => LCMMonoid (Union (Set a))
deriving newtype instance Ord a => LeftDistributiveGCDMonoid (Union (Set a))
deriving newtype instance Ord a => LeftGCDMonoid (Union (Set a))
deriving newtype instance Ord a => LeftReductive (Union (Set a))
deriving newtype instance Ord a => Monoid (Union (Set a))
deriving newtype instance Ord a => MonoidNull (Union (Set a))
deriving newtype instance Ord a => Monus (Union (Set a))
deriving newtype instance Ord a => OverlappingGCDMonoid (Union (Set a))
deriving newtype instance Ord a => PositiveMonoid (Union (Set a))
deriving newtype instance Ord a => Reductive (Union (Set a))
deriving newtype instance Ord a => RightDistributiveGCDMonoid (Union (Set a))
deriving newtype instance Ord a => RightGCDMonoid (Union (Set a))
deriving newtype instance Ord a => RightReductive (Union (Set a))
deriving newtype instance Ord a => Semigroup (Union (Set a))
{- ORMOLU_ENABLE -}
