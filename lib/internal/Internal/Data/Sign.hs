{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Data.Sign where

import Control.DeepSeq
    ( NFData
    )
import Data.Coerce
    ( coerce
    )
import Data.Group
    ( Abelian
    , Cyclic (generator)
    , Group (invert)
    )
import Data.Int
    ( Int16
    , Int32
    , Int64
    , Int8
    )
import Data.Monoid.Null
    ( MonoidNull (null)
    )
import Data.Semiring
    ( Semiring (one, plus, times, zero)
    )
import GHC.Generics
    ( Generic
    )
import Internal.Data.Monoid
    ( Product (Product)
    , Sum (Sum)
    )
import Prelude

--------------------------------------------------------------------------------
-- Sign
--------------------------------------------------------------------------------

data Sign
    = Negative
    | Positive
    deriving stock (Bounded, Eq, Generic, Ord, Read, Show)
    deriving anyclass (NFData)

--------------------------------------------------------------------------------
-- SignOrZero
--------------------------------------------------------------------------------

data SignOrZero
    = Sign !Sign
    | Zero
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving anyclass (NFData)

instance Bounded SignOrZero where
    minBound = Sign Negative
    maxBound = Sign Positive

{- ORMOLU_DISABLE -}
instance Semiring SignOrZero where
    zero = Zero
    one = Sign Positive
    plus a b = case (a, b) of
        (Zero         , x            ) -> x
        (x            , Zero         ) -> x
        (Sign Negative, Sign Positive) -> Zero
        (Sign Positive, Sign Negative) -> Zero
        (Sign Negative, Sign Negative) -> Sign Positive
        (Sign Positive, Sign Positive) -> Sign Negative
    times a b = case (a, b) of
        (Zero         , _            ) -> Zero
        (_            , Zero         ) -> Zero
        (Sign Positive, x            ) -> x
        (x            , Sign Positive) -> x
        (Sign Negative, Sign Negative) -> Sign Positive
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- SignOrZero: Sum
--------------------------------------------------------------------------------

instance Semigroup (Sum SignOrZero) where
    (<>) = coerce (plus @SignOrZero)

instance Monoid (Sum SignOrZero) where
    mempty = coerce Zero

instance MonoidNull (Sum SignOrZero) where
    null (Sum s) = s == Zero

instance Group (Sum SignOrZero) where
    invert (Sum s) = Sum $ case s of
        Sign Negative -> Sign Positive
        Sign Positive -> Sign Negative
        Zero -> Zero

instance Abelian (Sum SignOrZero)

instance Cyclic (Sum SignOrZero) where
    generator = coerce (Sign Positive)

--------------------------------------------------------------------------------
-- SignOrZero: Product
--------------------------------------------------------------------------------

instance Semigroup (Product SignOrZero) where
    (<>) = coerce (times @SignOrZero)

instance Monoid (Product SignOrZero) where
    mempty = coerce (Sign Positive)

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

assertNonZero :: SignOrZero -> Maybe Sign
assertNonZero = \case
    Sign x -> Just x
    Zero -> Nothing

forgetNonZero :: Sign -> SignOrZero
forgetNonZero = Sign

--------------------------------------------------------------------------------
-- Signed
--------------------------------------------------------------------------------

class Signed a where
    type SignOf a
    signOf :: a -> SignOf a

instance Signed Sign where
    type SignOf Sign = Sign
    signOf = id

instance Signed SignOrZero where
    type SignOf SignOrZero = SignOrZero
    signOf = id

newtype SignedNum a = SignedNum a

instance (Num a, Ord a) => Signed (SignedNum a) where
    type SignOf (SignedNum a) = SignOrZero
    signOf (SignedNum a)
        | a < 0 = Sign Negative
        | a > 0 = Sign Positive
        | otherwise = Zero

{- ORMOLU_DISABLE -}
deriving via SignedNum Integer instance Signed Integer
deriving via SignedNum Int     instance Signed Int
deriving via SignedNum Int8    instance Signed Int8
deriving via SignedNum Int16   instance Signed Int16
deriving via SignedNum Int32   instance Signed Int32
deriving via SignedNum Int64   instance Signed Int64
deriving via SignedNum Float   instance Signed Float
deriving via SignedNum Double  instance Signed Double
{- ORMOLU_ENABLE -}
