module Data.Multimap.Set where

import Data.MonoidMap
    ( MonoidMap
    )
import Data.Set
    ( Set
    )
import Prelude

newtype SetMultimap k v = SetMultimap (MonoidMap k (Set v))
    deriving Eq
