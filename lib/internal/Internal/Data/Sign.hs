{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal.Data.Sign where

import Control.DeepSeq
    ( NFData
    )
import GHC.Generics
    ( Generic
    )
import Prelude

data Sign
    = -- | Negative
      Negative
    | -- | Positive
      Positive
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
    deriving anyclass (NFData)

invert :: Sign -> Sign
invert Negative = Positive
invert Positive = Negative
