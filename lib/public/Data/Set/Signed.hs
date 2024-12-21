module Data.Set.Signed
    ( Sign (..)
    , SignedSet
    , empty
    , fromListWith
    , lookup
    , invert
    , union
    , unions1
    , intersection
    , intersections1
    )
where

import Data.Foldable1
    ( Foldable1
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Data.Sign
    ( Sign (..)
    )
import Internal.Shared
    ( SignedSet
    )
import Prelude hiding
    ( lookup
    )
import Data.Map.Strict (Map)

empty :: SignedSet a
empty = CountMap.empty

fromListWith :: Ord a => (Sign -> Sign -> Sign) -> [(a, Sign)] -> SignedSet a
fromListWith = CountMap.fromListWith

toList :: SignedSet a -> [(a, Sign)]
toList = CountMap.toList

fromMap :: Map a Sign -> SignedSet a
fromMap = CountMap.fromMap

toMap :: SignedSet a -> Map a Sign
toMap = CountMap.toMap

lookup :: Ord a => a -> SignedSet a -> Sign
lookup = CountMap.lookup

invert :: SignedSet a -> SignedSet a
invert = CountMap.invert

union :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
union = CountMap.union

intersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
intersection = CountMap.intersection

unions1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
unions1 = CountMap.unions1

intersections1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
intersections1 = CountMap.intersections1
