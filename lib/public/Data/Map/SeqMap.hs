module Data.Map.SeqMap where

import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Sequence
    ( Seq
    )
import Internal.Data.Packed
    ( Packed (Unpacked, unpack)
    )
import Prelude

newtype SeqMap k v = SeqMap (MonoidMap k (Seq v))

instance Packed (SeqMap k v) where
    type Unpacked (SeqMap k v) = MonoidMap k (Seq v)

isPrefixOf :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Bool
isPrefixOf m1 m2 = unpack m1 `MonoidMap.isPrefixOf` unpack m2

isProperPrefixOf :: SeqMap k v -> SeqMap k v -> Bool
isProperPrefixOf = undefined

isSuffixOf :: SeqMap k v -> SeqMap k v -> Bool
isSuffixOf = undefined

isProperSuffixOf :: SeqMap k v -> SeqMap k v -> Bool
isProperSuffixOf = undefined
