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
    , symmetricPowersetElements
    , isSymmetricSubsetOf
    )
where

import Data.Foldable1
    ( Foldable1
    )
import Data.Map.Strict
    ( Map
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

isLessThan :: Ord a => SignedSet a -> SignedSet a -> Bool
isLessThan = CountMap.isLessThan

isLessThanOrEqualTo :: Ord a => SignedSet a -> SignedSet a -> Bool
isLessThanOrEqualTo = CountMap.isLessThanOrEqualTo

isGreaterThan :: Ord a => SignedSet a -> SignedSet a -> Bool
isGreaterThan = CountMap.isGreaterThan

isGreaterThanOrEqualTo :: Ord a => SignedSet a -> SignedSet a -> Bool
isGreaterThanOrEqualTo = CountMap.isGreaterThanOrEqualTo

isSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSubsetOf = CountMap.isSubmapOf

isProperSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSubsetOf = CountMap.isProperSubmapOf

isSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSupersetOf = CountMap.isSupermapOf

isProperSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSupersetOf = CountMap.isProperSupermapOf

isSymmetricSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSymmetricSubsetOf = CountMap.isSymmetricSubmapOf

isProperSymmetricSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSymmetricSubsetOf = CountMap.isProperSymmetricSubmapOf

isSymmetricSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSymmetricSupersetOf = CountMap.isSymmetricSupermapOf

isProperSymmetricSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSymmetricSupersetOf = CountMap.isProperSymmetricSupermapOf

symmetricPowersetElements :: Ord a => SignedSet a -> [SignedSet a]
symmetricPowersetElements = CountMap.symmetricPowersetElements
