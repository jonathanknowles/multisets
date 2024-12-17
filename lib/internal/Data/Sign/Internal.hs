{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Data.Sign.Internal
    ( Sign (..)
    , Min (..)
    , Max (..)
    , Sum (..)
    , Product (..)
    , integralToSign
    )
where

import Data.Coerce
    ( coerce
    )
import Data.Group
    ( Abelian
    , Cyclic (generator)
    , Group
    )
import Data.Group qualified as Group
    ( Group (..)
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.Monoid.Null qualified as MonoidNull
import Data.Semigroup
    ( Max (..)
    , Min (..)
    )
import Prelude hiding
    ( null
    )

data Sign
    = Negative
    | Zero
    | Positive
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

--------------------------------------------------------------------------------
-- Sum
--------------------------------------------------------------------------------

newtype Sum a = Sum {getSum :: a}
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)

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

newtype Product a = Product {getProduct :: a}
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)

instance Semigroup (Product Sign) where
    (<>) = coerce multiply

instance Monoid (Product Sign) where
    mempty = coerce Positive

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

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
-- Model
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
model_add :: Sign -> Sign -> Sign
model_add s1 s2
    = integralToSign @Int
    $ (`moduloInclusiveRange` (-1, 1))
    $ signToIntegral s1 + signToIntegral s2
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
model_multiply :: Sign -> Sign -> Sign
model_multiply s1 s2
    = integralToSign @Int
    $ signToIntegral s1 * signToIntegral s2
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

moduloInclusiveRange :: Integral i => i -> (i, i) -> i
moduloInclusiveRange i (lo, hi) = ((i - lo) `mod` (hi - lo + 1)) + lo

{- ORMOLU_DISABLE -}
signToIntegral :: Integral i => Sign -> i
signToIntegral = \case
    Negative -> -1
    Zero     ->  0
    Positive ->  1
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
integralToSign :: Integral i => i -> Sign
integralToSign i
    | i < 0     = Negative
    | i > 0     = Positive
    | otherwise = Zero
{- ORMOLU_ENABLE -}
