module Data.Sign.Internal
    where

import Prelude

data Sign
    = SignNegative
    | SignZero
    | SignPositive
    deriving stock (Bounded, Enum, Eq, Ord, Show)
