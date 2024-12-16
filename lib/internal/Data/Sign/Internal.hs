{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Data.Sign.Internal where

import Data.Coerce
    ( coerce
    )
import Data.Function
    ( on
    )
import Data.Group
    ( Cyclic (generator)
    , Group
    )
import Data.Group qualified as Group
    ( Group (..)
    )
import Data.Monoid.Cancellative
    ( Commutative
    )
import Prelude

newtype Sum a = Sum a
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)

newtype Product a = Product a
    deriving newtype (Bounded, Enum, Eq, Ord)
    deriving stock (Read, Show)

--------------------------------------------------------------------------------
-- 2-element signs
--------------------------------------------------------------------------------

data StrictSign
    = StrictNegative
    | StrictPositive
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

instance Semigroup (Product StrictSign) where
    (<>) = coerce multiply

instance Monoid (Product StrictSign) where
    mempty = coerce StrictPositive

instance Group (Product StrictSign) where
    invert = id

instance Cyclic (Product StrictSign) where
    generator = coerce StrictNegative

multiply :: StrictSign -> StrictSign -> StrictSign
multiply StrictNegative StrictNegative = StrictPositive
multiply StrictNegative StrictPositive = StrictNegative
multiply StrictPositive StrictNegative = StrictNegative
multiply StrictPositive StrictPositive = StrictPositive

--------------------------------------------------------------------------------
-- 3-element signs
--------------------------------------------------------------------------------

data Sign
    = Negative
    | Zero
    | Positive
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

instance Semigroup (Sum Sign) where
    (<>) = coerce add

instance Commutative (Sum Sign)

instance Monoid (Sum Sign) where
    mempty = coerce Zero

instance Group (Sum Sign) where
    invert = coerce invert

instance Cyclic (Sum Sign) where
    generator = coerce Positive

{- ORMOLU_DISABLE -}
add :: Sign -> Sign -> Sign
add Negative Negative = Positive
add Negative Zero     = Negative
add Negative Positive = Zero
add Zero     Negative = Negative
add Zero     Zero     = Zero
add Zero     Positive = Positive
add Positive Negative = Zero
add Positive Zero     = Positive
add Positive Positive = Negative
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
invert :: Sign -> Sign
invert = \case
    Negative -> Positive
    Zero     -> Zero
    Positive -> Negative
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

{- ORMOLU_DISABLE -}
model_add :: Sign -> Sign -> Sign
model_add s1 s2
    = integralToSign @Int
    $ subtract 1
    $ (`mod` 3)
    $ (+ 1)
    $ ((+) `on` signToIntegral) s1 s2
{- ORMOLU_ENABLE -}

model_multiply :: Sign -> Sign -> Sign
model_multiply s1 s2 =
    integralToSign @Int $
        ((*) `on` signToIntegral) s1 s2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

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
