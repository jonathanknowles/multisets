module Data.Signed where

class Signed s where
    type Unsigned s
    negativePart :: s -> Unsigned s
    positivePart :: s -> Unsigned s
    splitToUnsigned :: s -> (Unsigned s, Unsigned s)
    mergeOfUnsigned :: (Unsigned s, Unsigned s) -> s

