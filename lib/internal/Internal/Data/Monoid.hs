{-# LANGUAGE StandaloneDeriving #-}

module Internal.Data.Monoid
    ( Sum (..)
    , Product (..)
    , Min (..)
    , Max (..)
    )
where

import Data.Monoid qualified as B
import Data.Monoid.Cancellative
    ( LeftReductive
    )
import Data.Monoid.Monus
    ( Monus
    , OverlappingGCDMonoid
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.Semigroup
    ( Max (..)
    , Min (..)
    )
import Data.Semigroup.Cancellative
    ( Commutative
    , RightReductive
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Sum a = Sum {getSum :: a}
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)

newtype Product a = Product {getProduct :: a}
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)

{- ORMOLU_DISABLE -}
deriving via B.Product Natural instance Semigroup            (Product Natural)
deriving via B.Product Natural instance Monoid               (Product Natural)
deriving via B.Product Natural instance MonoidNull           (Product Natural)
deriving via B.Product Natural instance Commutative          (Product Natural)
deriving via B.Product Natural instance LeftReductive        (Product Natural)
deriving via B.Product Natural instance RightReductive       (Product Natural)
deriving via B.Product Natural instance OverlappingGCDMonoid (Product Natural)
deriving via B.Product Natural instance Monus                (Product Natural)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via B.Product Integer instance Semigroup            (Product Integer)
deriving via B.Product Integer instance Monoid               (Product Integer)
deriving via B.Product Integer instance MonoidNull           (Product Integer)
deriving via B.Product Integer instance Commutative          (Product Integer)
deriving via B.Product Integer instance LeftReductive        (Product Integer)
deriving via B.Product Integer instance RightReductive       (Product Integer)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via B.Sum Natural instance Semigroup            (Sum Natural)
deriving via B.Sum Natural instance Monoid               (Sum Natural)
deriving via B.Sum Natural instance MonoidNull           (Sum Natural)
deriving via B.Sum Natural instance Commutative          (Sum Natural)
deriving via B.Sum Natural instance LeftReductive        (Sum Natural)
deriving via B.Sum Natural instance RightReductive       (Sum Natural)
deriving via B.Sum Natural instance OverlappingGCDMonoid (Sum Natural)
deriving via B.Sum Natural instance Monus                (Sum Natural)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via B.Sum Integer instance Semigroup            (Sum Integer)
deriving via B.Sum Integer instance Monoid               (Sum Integer)
deriving via B.Sum Integer instance MonoidNull           (Sum Integer)
deriving via B.Sum Integer instance Commutative          (Sum Integer)
deriving via B.Sum Integer instance LeftReductive        (Sum Integer)
deriving via B.Sum Integer instance RightReductive       (Sum Integer)
{- ORMOLU_ENABLE -}
