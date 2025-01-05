{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Internal.Data.Sign
    ( Sign (..)
    , fromNum
    , HasSign (..)
    , HasMagnitude (..)
    )
where

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
import Internal.Data.Monoid
    ( Product (..)
    , Sum (..)
    )
import Numeric.Natural (Natural)
import Prelude hiding
    ( null
    )
import Data.Word (Word8, Word16, Word32, Word64)

data Sign
    = Negative
    | Zero
    | Positive
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

instance Semiring Sign where
    zero = Zero
    one = Positive
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
    mempty = coerce Zero

instance MonoidNull (Sum Sign) where
    null = coerce null

instance Group (Sum Sign) where
    invert = coerce invert

instance Abelian (Sum Sign)

instance Cyclic (Sum Sign) where
    generator = coerce Positive

--------------------------------------------------------------------------------
-- Product
--------------------------------------------------------------------------------

instance Semigroup (Product Sign) where
    (<>) = coerce multiply

instance Monoid (Product Sign) where
    mempty = coerce Positive

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
fromNum :: (Num n, Ord n) => n -> Sign
fromNum n
    | n < 0     = Negative
    | n > 0     = Positive
    | otherwise = Zero
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
null :: Sign -> Bool
null Zero = True
null _    = False
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
invert :: Sign -> Sign
invert Zero     = Zero
invert Negative = Positive
invert Positive = Negative
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
add :: Sign -> Sign -> Sign
add Zero     x        = x
add x        Zero     = x
add Negative Positive = Zero
add Positive Negative = Zero
add Negative Negative = Positive
add Positive Positive = Negative
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
multiply :: Sign -> Sign -> Sign
multiply Zero     _        = Zero
multiply _        Zero     = Zero
multiply Positive x        = x
multiply x        Positive = x
multiply Negative Negative = Positive
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- HasSign
--------------------------------------------------------------------------------

class HasSign a where
    signOf :: a -> Sign

newtype HasSignNumEq a = HasSignNumEq a

{- ORMOLU_DISABLE -}
instance (Num a, Ord a) => HasSign (HasSignNumEq a) where
    signOf (HasSignNumEq a) = case signum a of
        (-1) -> Negative
        ( 0) -> Zero
        ( 1) -> Positive
        ( _) -> error "HasSignNumEq: signnum post-condition violated"
{- ORMOLU_ENABLE -}

newtype HasSignNumOrd a = HasSignNumOrd a

instance (Num a, Ord a) => HasSign (HasSignNumOrd a) where
    signOf (HasSignNumOrd a)
        | i < 0 = Negative
        | i > 0 = Positive
        | otherwise = Zero
      where
        i = signum a

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

--------------------------------------------------------------------------------
-- Magnitude
--------------------------------------------------------------------------------

class HasMagnitude i where
    type Magnitude i
    magnitude :: i -> Magnitude i

instance HasMagnitude Integer where
    type Magnitude Integer = Natural
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int where
    type Magnitude Int = Word
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int8 where
    type Magnitude Int8 = Word8
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int16 where
    type Magnitude Int16 = Word16
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int32 where
    type Magnitude Int32 = Word32
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int64 where
    type Magnitude Int64 = Word64
    magnitude = magnitudeIntegralNum

magnitudeIntegralNum :: (Integral a, Num b) => a -> b
magnitudeIntegralNum i = fromIntegral $ abs i
