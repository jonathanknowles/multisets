module Data.Sign.Internal
    where

import Prelude

data Sign
    = Negative
    | Zero
    | Positive
    deriving stock (Bounded, Enum, Eq, Ord, Show)

{- ORMOLU_DISABLE -}
multiply :: Sign -> Sign -> Sign
multiply = \case
    Negative -> invert
    Zero     -> const Zero
    Positive -> id
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
invert :: Sign -> Sign
invert = \case
    Negative -> Positive
    Zero     -> Zero
    Positive -> Negative
{- ORMOLU_ENABLE -}
