{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Internal.Data.Sign
    ( Sign (..)
    , HasSign (..)
    , toNonZero
    , fromNonZero
    )
where

import Control.DeepSeq
    ( NFData
    )
import Data.Coerce
    ( coerce
    )
import Data.Functor.Identity
    ( Identity (Identity)
    )
import Data.Group
    ( Abelian
    , Cyclic (generator)
    , Group
    )
import Data.Group qualified as Group
    ( Group (..)
    )
import Data.Int
    ( Int16
    , Int32
    , Int64
    , Int8
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.Monoid.Null qualified as MonoidNull
import Data.Semigroup
    ( Max (Max)
    , Min (Min)
    )
import Data.Semiring
    ( Ring (..)
    , Semiring (..)
    )
import GHC.Generics
    ( Generic
    )
import Internal.Data.Monoid
    ( Product (..)
    , Sum (..)
    )
import Internal.Data.Sign.NonZero qualified as NonZero
import Prelude hiding
    ( null
    )

data Sign
    = -- | Negative
      N
    | -- | Zero
      Z
    | -- | Positive
      P
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
    deriving anyclass (NFData)

instance Semiring Sign where
    zero = Z
    one = P
    plus = add
    times = multiply

instance Ring Sign where
    negate = invert

--------------------------------------------------------------------------------
-- Sum
--------------------------------------------------------------------------------

instance Semigroup (Sum Sign) where
    (<>) = coerce add

instance Monoid (Sum Sign) where
    mempty = coerce Z

instance MonoidNull (Sum Sign) where
    null = coerce null

instance Group (Sum Sign) where
    invert = coerce invert

instance Abelian (Sum Sign)

instance Cyclic (Sum Sign) where
    generator = coerce P

--------------------------------------------------------------------------------
-- Product
--------------------------------------------------------------------------------

instance Semigroup (Product Sign) where
    (<>) = coerce multiply

instance Monoid (Product Sign) where
    mempty = coerce P

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

toNonZero :: Sign -> Maybe NonZero.Sign
toNonZero = \case
    N -> Just NonZero.N
    P -> Just NonZero.P
    Z -> Nothing

fromNonZero :: NonZero.Sign -> Sign
fromNonZero = \case
    NonZero.N -> N
    NonZero.P -> P

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

null :: Sign -> Bool
null Z = True
null _ = False

invert :: Sign -> Sign
invert N = P
invert Z = Z
invert P = N

add :: Sign -> Sign -> Sign
add Z x = x
add x Z = x
add N P = Z
add P N = Z
add N N = P
add P P = N

multiply :: Sign -> Sign -> Sign
multiply Z _ = Z
multiply _ Z = Z
multiply P x = x
multiply x P = x
multiply N N = P

--------------------------------------------------------------------------------
-- HasSign
--------------------------------------------------------------------------------

class HasSign a where
    signOf :: a -> Sign

newtype HasSignNumEq a = HasSignNumEq a

instance HasSign Sign where
    signOf = id

{- ORMOLU_DISABLE -}
instance (Num a, Ord a) => HasSign (HasSignNumEq a) where
    signOf (HasSignNumEq a) = case signum a of
        (-1) -> N
        ( 0) -> Z
        ( 1) -> P
        ( _) -> error "HasSignNumEq: signnum post-condition violated"
{- ORMOLU_ENABLE -}

newtype HasSignNumOrd a = HasSignNumOrd a

{- ORMOLU_DISABLE -}
instance (Num a, Ord a) => HasSign (HasSignNumOrd a) where
    signOf (HasSignNumOrd a)
        | i < 0     = N
        | i > 0     = P
        | otherwise = Z
      where
        i = signum a
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via HasSignNumEq  Integer instance HasSign Integer
deriving via HasSignNumEq  Int     instance HasSign Int
deriving via HasSignNumEq  Int8    instance HasSign Int8
deriving via HasSignNumEq  Int16   instance HasSign Int16
deriving via HasSignNumEq  Int32   instance HasSign Int32
deriving via HasSignNumEq  Int64   instance HasSign Int64
deriving via HasSignNumOrd Float   instance HasSign Float
deriving via HasSignNumOrd Double  instance HasSign Double
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving newtype instance HasSign a => HasSign (Identity a)
deriving newtype instance HasSign a => HasSign (Max a)
deriving newtype instance HasSign a => HasSign (Min a)
{- ORMOLU_ENABLE -}
