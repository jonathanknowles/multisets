{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Internal.Data.Sign
    ( Sign (..)
    , fromNum
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
import Data.Semiring
    ( Ring (..)
    , Semiring (..)
    )
import Internal.Data.Monoid
    ( Product (..)
    , Sum (..)
    )
import Prelude hiding
    ( fromIntegral
    , null
    )

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
