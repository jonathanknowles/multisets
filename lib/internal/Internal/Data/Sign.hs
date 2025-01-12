{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Internal.Data.Sign
    ( Sign (..)
    , fromNum
    , HasSign (..)
    , HasMagnitude (..)
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
import Data.Word
    ( Word16
    , Word32
    , Word64
    , Word8
    )
import GHC.Generics
    ( Generic
    )
import Internal.Data.Monoid
    ( Product (..)
    , Sum (..)
    )
import Numeric.Natural
    ( Natural
    )
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
-- Functions
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
fromNum :: (Num n, Ord n) => n -> Sign
fromNum n
    | n < 0     = N
    | n > 0     = P
    | otherwise = Z
{- ORMOLU_ENABLE -}

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

--------------------------------------------------------------------------------
-- Magnitude
--------------------------------------------------------------------------------

class HasMagnitude i where
    type Magnitude i
    magnitude :: i -> Magnitude i

instance HasMagnitude Sign where
    type Magnitude Sign = Bool
    magnitude = \case
        N -> True
        Z -> False
        P -> True

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
