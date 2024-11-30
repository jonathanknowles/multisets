module Data.Set.Signed
    ( SignedSet
    , fromListWith
    , toList
    )
where

import Data.Bag.Internal
    ( Count (Count)
    , SignedSet (SignedSet)
    )
import Data.Coerce
    ( coerce
    )
import Data.Sign.Internal
    ( Sign (..)
    )
import Data.MonoidMap qualified as MonoidMap
import Prelude

fromListWith
    :: Ord a
    => (Sign -> Sign -> Sign)
    -> [(a, Sign)]
    -> SignedSet a
fromListWith f =
    SignedSet
        . MonoidMap.fromListWith (coerce f)
        -- use coerce
        . fmap (fmap Count)

toList :: SignedSet a -> [(a, Sign)]
toList (SignedSet s) = coerce (MonoidMap.toList s)
