module Data.Set.Signed
    ( -- * Types
      Sign (..)
    , SignedSet

      -- * Construction
    , empty
    , singleton
    , fromListWith
    , fromMap
    , fromSet

      -- * Deconstruction
    , toList
    , toMap
    , toSet

      -- * Membership
    , null
    , count
    , member

      -- * Indication
    , isRegular
    , isSimple
    , maybeRegular
    , maybeSimple

      -- * Folding
    , foldl
    , foldl'
    , foldr
    , foldr'
    , foldMap
    , foldMap'

      -- * Mapping
    , mapWith
    , mapSigns

      -- * Transformation
    , invert

      -- * Algebra
    , union
    , unionMany1
    , intersection
    , intersectionMany1

      -- * Comparison
    , compareLexically
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

      -- * Combinatorics
    , symmetricPowerset
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
    ( foldMap
    , foldl
    , foldl'
    , foldr
    , map
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

toSet :: SignedSet a -> Set a
toSet = CountMap.toSet

null :: SignedSet a -> Bool
null = CountMap.null

count :: Ord a => a -> SignedSet a -> Sign
count = CountMap.count

member :: Ord a => a -> SignedSet a -> Bool
member = CountMap.member

isRegular :: Ord a => SignedSet a -> Bool
isRegular = CountMap.isRegular

isSimple :: Ord a => SignedSet a -> Bool
isSimple = CountMap.isSimple

maybeRegular :: Ord a => SignedSet a -> Maybe (Sign, Set a)
maybeRegular = CountMap.maybeRegular

maybeSimple :: Ord a => SignedSet a -> Maybe (Sign, a)
maybeSimple = CountMap.maybeSimple

foldl :: (r -> a -> Sign -> r) -> r -> SignedSet a -> r
foldl = CountMap.foldl

foldl' :: (r -> a -> Sign -> r) -> r -> SignedSet a -> r
foldl' = CountMap.foldl'

foldr :: (a -> Sign -> r -> r) -> r -> SignedSet a -> r
foldr = CountMap.foldr

foldr' :: (a -> Sign -> r -> r) -> r -> SignedSet a -> r
foldr' = CountMap.foldr'

foldMap :: Monoid m => (a -> Sign -> m) -> SignedSet a -> m
foldMap = CountMap.foldMap

foldMap' :: Monoid m => (a -> Sign -> m) -> SignedSet a -> m
foldMap' = CountMap.foldMap'

mapWith
    :: Ord b
    => (Sign -> Sign -> Sign)
    -> (a -> b)
    -> SignedSet a
    -> SignedSet b
mapWith = CountMap.mapWith

mapSigns :: (Sign -> Sign) -> SignedSet a -> SignedSet a
mapSigns = CountMap.mapCounts

invert :: SignedSet a -> SignedSet a
invert = CountMap.invert

union :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
union = CountMap.union

unionMany1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
unionMany1 = CountMap.unionMany1

intersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
intersection = CountMap.intersection

intersectionMany1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
intersectionMany1 = CountMap.intersectionMany1

compareLexically :: Ord a => SignedSet a -> SignedSet a -> Ordering
compareLexically = CountMap.compareLexically

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

symmetricPowerset :: Ord a => SignedSet a -> Set (SignedSet a)
symmetricPowerset = CountMap.symmetricPowerset

symmetricPowersetElements :: Ord a => SignedSet a -> [SignedSet a]
symmetricPowersetElements = CountMap.symmetricPowersetElements

symmetricPowersetSize :: Ord a => SignedSet a -> Natural
symmetricPowersetSize = CountMap.symmetricPowersetSize
