module Data.Multimap.Seq where

import Data.MonoidMap
    ( MonoidMap
    )
import Data.Sequence
    ( Seq
    )
import Prelude

newtype SeqMultimap k v = SeqMultimap (MonoidMap k (Seq v))
    deriving (Eq)
