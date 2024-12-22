{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Internal.Data.Count where

import Data.Group
    ( Group
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Internal.Data.Monoid
    ( Sum (Sum)
    )
import Internal.Data.Packed
    ( Packed (Unpacked)
    )
import Internal.Data.Sign
    ( Sign (Negative, Positive, Zero)
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Count a = Count a
    deriving stock (Eq, Ord, Functor)

instance Packed (Count a) where
    type Unpacked (Count a) = a

{- ORMOLU_DISABLE -}
deriving via Sum Sign instance Semigroup  (Count Sign)
deriving via Sum Sign instance Monoid     (Count Sign)
deriving via Sum Sign instance MonoidNull (Count Sign)
deriving via Sum Sign instance Group      (Count Sign)
{- ORMOLU_ENABLE -}

deriving instance Enum (Count Sign)

class CountMagnitude c where
    countToInteger :: Count c -> Integer
    countToNatural :: Count c -> Natural

{- ORMOLU_DISABLE -}
instance CountMagnitude Integer where
    countToInteger (Count i) = i
    countToNatural (Count i) = fromIntegral (abs i)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
instance CountMagnitude Natural where
    countToInteger (Count n) = fromIntegral n
    countToNatural (Count n) = n
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
instance CountMagnitude Sign where
    countToInteger (Count s) = case s of
        Negative -> -1
        Zero     ->  0
        Positive ->  1
    countToNatural (Count s) = case s of
        Negative ->  1
        Zero     ->  0
        Positive ->  1
{- ORMOLU_ENABLE -}
