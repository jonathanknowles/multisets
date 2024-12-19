module Internal.Data.Monoid
    ( Sum (..)
    , Product (..)
    , Min (..)
    , Max (..)
    )
where

import Data.Semigroup
    ( Max (..)
    , Min (..)
    )
import Prelude

newtype Sum a = Sum {getSum :: a}
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)

newtype Product a = Product {getProduct :: a}
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)
