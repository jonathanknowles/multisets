{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Data.Sign.NonZero where

import Control.DeepSeq
    ( NFData
    )
import GHC.Generics
    ( Generic
    )
import Prelude

data Sign
    = -- | Negative
      N
    | -- | Positive
      P
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
    deriving anyclass (NFData)

invert :: Sign -> Sign
invert N = P
invert P = N
