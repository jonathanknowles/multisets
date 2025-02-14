{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Internal.Data.Sign.Num
    ( NumSign (..)
    , SignNum (..)
    , toSign
    , fromSign
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
import Internal.Data.Sign
    ( Sign
    )
import Internal.Data.Sign qualified as Sign
import Prelude hiding
    ( null
    )

data NumSign
    = -- | Negative
      N
    | -- | Zero
      Z
    | -- | Positive
      P
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
    deriving anyclass (NFData)

instance Semiring NumSign where
    zero = Z
    one = P
    plus = add
    times = multiply

instance Ring NumSign where
    negate = invert

--------------------------------------------------------------------------------
-- Sum
--------------------------------------------------------------------------------

instance Semigroup (Sum NumSign) where
    (<>) = coerce add

instance Monoid (Sum NumSign) where
    mempty = coerce Z

instance MonoidNull (Sum NumSign) where
    null = coerce null

instance Group (Sum NumSign) where
    invert = coerce invert

instance Abelian (Sum NumSign)

instance Cyclic (Sum NumSign) where
    generator = coerce P

--------------------------------------------------------------------------------
-- Product
--------------------------------------------------------------------------------

instance Semigroup (Product NumSign) where
    (<>) = coerce multiply

instance Monoid (Product NumSign) where
    mempty = coerce P

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

toSign :: NumSign -> Maybe Sign
toSign = \case
    N -> Just Sign.Negative
    P -> Just Sign.Positive
    Z -> Nothing

fromSign :: Sign -> NumSign
fromSign = \case
    Sign.Negative -> N
    Sign.Positive -> P

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

null :: NumSign -> Bool
null Z = True
null _ = False

invert :: NumSign -> NumSign
invert N = P
invert Z = Z
invert P = N

add :: NumSign -> NumSign -> NumSign
add Z x = x
add x Z = x
add N P = Z
add P N = Z
add N N = P
add P P = N

multiply :: NumSign -> NumSign -> NumSign
multiply Z _ = Z
multiply _ Z = Z
multiply P x = x
multiply x P = x
multiply N N = P

--------------------------------------------------------------------------------
-- SignNum
--------------------------------------------------------------------------------

class SignNum a where
    signNum :: a -> NumSign

newtype SignNumEq a = SignNumEq a

instance SignNum NumSign where
    signNum = id

{- ORMOLU_DISABLE -}
instance (Num a, Ord a) => SignNum (SignNumEq a) where
    signNum (SignNumEq a) = case signum a of
        (-1) -> N
        ( 0) -> Z
        ( 1) -> P
        ( _) -> error "SignNumEq: signnum post-condition violated"
{- ORMOLU_ENABLE -}

newtype SignNumOrd a = SignNumOrd a

{- ORMOLU_DISABLE -}
instance (Num a, Ord a) => SignNum (SignNumOrd a) where
    signNum (SignNumOrd a)
        | i < 0     = N
        | i > 0     = P
        | otherwise = Z
      where
        i = signum a
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via SignNumEq  Integer instance SignNum Integer
deriving via SignNumEq  Int     instance SignNum Int
deriving via SignNumEq  Int8    instance SignNum Int8
deriving via SignNumEq  Int16   instance SignNum Int16
deriving via SignNumEq  Int32   instance SignNum Int32
deriving via SignNumEq  Int64   instance SignNum Int64
deriving via SignNumOrd Float   instance SignNum Float
deriving via SignNumOrd Double  instance SignNum Double
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving newtype instance SignNum a => SignNum (Identity a)
deriving newtype instance SignNum a => SignNum (Max a)
deriving newtype instance SignNum a => SignNum (Min a)
{- ORMOLU_ENABLE -}
