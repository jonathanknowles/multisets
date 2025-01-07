module Data.Set.Signed
    ( Sign (..)
    , SignedSet
    , empty
    , singleton
    , fromListWith
    , fromMap
    , fromSet
    , toList
    , toMap
    , null
    , lookup
    , member
    , foldl
    , foldl'
    , foldr
    , foldr'
    , invert
    , union
    , unions1
    , intersection
    , intersections1
    , isLessThan
    , isLessThanOrEqualTo
    , isGreaterThan
    , isGreaterThanOrEqualTo
    , isSubsetOf
    , isSupersetOf
    , isProperSubsetOf
    , isProperSupersetOf
    , isSymmetricSubsetOf
    , isSymmetricSupersetOf
    , isProperSymmetricSubsetOf
    , isProperSymmetricSupersetOf
    , symmetricPowersetElements
    , symmetricPowersetSize
    )
where

import Data.Foldable1
    ( Foldable1
    )
import Data.Map.Strict
    ( Map
    )
import Data.Set
    ( Set
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Data.Sign
    ( Sign (..)
    )
import Internal.Shared
    ( SignedSet
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( foldl
    , foldl'
    , foldr
    , lookup
    , null
    )

empty :: SignedSet a
empty = CountMap.empty

singleton :: Ord a => a -> SignedSet a
singleton = CountMap.singleton

fromListWith :: Ord a => (Sign -> Sign -> Sign) -> [(a, Sign)] -> SignedSet a
fromListWith = CountMap.fromListWith

fromMap :: Map a Sign -> SignedSet a
fromMap = CountMap.fromMap

fromSet :: (a -> Sign) -> Set a -> SignedSet a
fromSet = CountMap.fromSet

toList :: SignedSet a -> [(a, Sign)]
toList = CountMap.toList

toMap :: SignedSet a -> Map a Sign
toMap = CountMap.toMap

null :: SignedSet a -> Bool
null = CountMap.null

lookup :: Ord a => a -> SignedSet a -> Sign
lookup = CountMap.lookup

member :: Ord a => a -> SignedSet a -> Bool
member = CountMap.member

foldl :: (r -> a -> Sign -> r) -> r -> SignedSet a -> r
foldl = CountMap.foldl

foldl' :: (r -> a -> Sign -> r) -> r -> SignedSet a -> r
foldl' = CountMap.foldl'

foldr :: (a -> Sign -> r -> r) -> r -> SignedSet a -> r
foldr = CountMap.foldr

foldr' :: (a -> Sign -> r -> r) -> r -> SignedSet a -> r
foldr' = CountMap.foldr'

invert :: SignedSet a -> SignedSet a
invert = CountMap.invert

union :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
union = CountMap.union

unions1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
unions1 = CountMap.unions1

intersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
intersection = CountMap.intersection

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

isSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSupersetOf = CountMap.isSupermapOf

isProperSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSubsetOf = CountMap.isProperSubmapOf

isProperSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSupersetOf = CountMap.isProperSupermapOf

isSymmetricSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSymmetricSubsetOf = CountMap.isSymmetricSubmapOf

isSymmetricSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSymmetricSupersetOf = CountMap.isSymmetricSupermapOf

isProperSymmetricSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSymmetricSubsetOf = CountMap.isProperSymmetricSubmapOf

isProperSymmetricSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSymmetricSupersetOf = CountMap.isProperSymmetricSupermapOf

symmetricPowersetElements :: Ord a => SignedSet a -> [SignedSet a]
symmetricPowersetElements = CountMap.symmetricPowersetElements

symmetricPowersetSize :: Ord a => SignedSet a -> Natural
symmetricPowersetSize = CountMap.symmetricPowersetSize
