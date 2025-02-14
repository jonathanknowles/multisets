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
    ( Ring (negate)
    , Semiring (one, plus, times, zero)
    )
import GHC.Generics
    ( Generic
    )
import Internal.Data.Monoid
    ( Product (Product)
    , Sum (Sum)
    )
import Prelude hiding
    ( negate
    )

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
    deriving stock (Eq, Generic, Read, Show)
    deriving anyclass (NFData)

instance Bounded SignOrZero where
    minBound = Sign Negative
    maxBound = Sign Positive

-- TODO:
-- Eliminate this instance. There's no way to create a safe, idiomatic instance.
instance Enum SignOrZero where
    toEnum i = case i `moduloInclusiveRange` (-1, 1) of
        -1 -> Sign Negative
        01 -> Sign Positive
        00 -> Zero
        __ -> errorInvalidEnumSignOrZero
    fromEnum = \case
        Sign Negative -> -1
        Sign Positive -> 01
        Zero -> 0

instance Ord SignOrZero where
    compare s1 s2 = case (s1, s2) of
        (Sign Negative, Sign Negative) -> EQ
        (Sign Negative, _) -> LT
        (Sign Positive, Sign Positive) -> EQ
        (Sign Positive, _) -> GT
        (Zero, Sign Negative) -> GT
        (Zero, Sign Positive) -> LT
        (Zero, Zero) -> EQ

moduloInclusiveRange :: Integral i => i -> (i, i) -> i
moduloInclusiveRange i (lo, hi) = ((i - lo) `mod` (hi - lo + 1)) + lo

errorInvalidEnumSignOrZero :: a
errorInvalidEnumSignOrZero = error "Invalid Enum value for SignOrzero"

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

instance Ring SignOrZero where
    negate = \case
        Sign Negative -> Sign Positive
        Sign Positive -> Sign Negative
        Zero -> Zero

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
    invert = coerce (negate @SignOrZero)

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
