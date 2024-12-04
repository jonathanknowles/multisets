module Data.Sign.Internal
    where

import Prelude

data Sign
    = Negative
    | Zero
    | Positive
    deriving stock (Bounded, Enum, Eq, Ord, Show)
