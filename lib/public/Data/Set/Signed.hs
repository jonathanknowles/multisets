module Data.Set.Signed
    ( SignedSet
    , fromListWith
    , lookup
    , invert
    , union
    , unions
    , intersection
    , intersections
    )
where

import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Internal.Data.Sign
    ( Sign (..)
    )
import Internal.Shared
    ( SignedSet
    )
import Internal.Shared qualified as Internal
import Prelude hiding
    ( lookup
    )

fromListWith :: Ord a => (Sign -> Sign -> Sign) -> [(a, Sign)] -> SignedSet a
fromListWith = Internal.fromListWith

lookup :: Ord a => a -> SignedSet a -> Sign
lookup = Internal.lookup

invert :: SignedSet a -> SignedSet a
invert = Internal.invert

union :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
union = Internal.union

intersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
intersection = Internal.intersection

unions :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
unions = Foldable1.foldl1' union

intersections :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
intersections = Foldable1.foldl1' union
