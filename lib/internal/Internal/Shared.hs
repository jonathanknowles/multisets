{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Shared where

import Data.Group
    ( Group
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Internal.Data.CountMap
    ( Count (..)
    , CountMap
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Data.Monoid
    ( Sum (..)
    )
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

{- ORMOLU_DISABLE -}
deriving via Sum Sign instance Semigroup  (Count Sign)
deriving via Sum Sign instance Monoid     (Count Sign)
deriving via Sum Sign instance MonoidNull (Count Sign)
deriving via Sum Sign instance Group      (Count Sign)
deriving instance Enum (Count Sign)
{- ORMOLU_ENABLE -}

newtype Bag a = Bag (CountMap a Natural)
    deriving newtype (Eq)

newtype SignedBag a = SignedBag (CountMap a Integer)
    deriving newtype (Eq)

newtype SignedSet a = SignedSet (CountMap a Sign)
    deriving newtype (Eq)

instance Packed (Bag a) where
    type Unpacked (Bag a) = CountMap a Natural

instance Packed (SignedBag a) where
    type Unpacked (SignedBag a) = CountMap a Integer

instance Packed (SignedSet a) where
    type Unpacked (SignedSet a) = CountMap a Sign

instance Show a => Show (Bag a) where
    show = CountMap.showWith "Bag" "(+)"

instance Show a => Show (SignedBag a) where
    show = CountMap.showWith "SignedBag" "(+)"

instance Show a => Show (SignedSet a) where
    show = CountMap.showWith "SignedSet" "Sign.add"
