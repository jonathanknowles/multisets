module Data.Set.Signed
    ( -- * Types
      SignedSet

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
    , lookup
    , member
    , null

      -- * Indication
    , isRegular
    , isSimple
    , isSingleton
    , isSingletonSigned
    , isUnipolar
    , isBipolar
    , isNegative
    , isPositive

      -- * Projection
    , maybeRegular
    , maybeSimple
    , maybeSingleton
    , maybeSingletonSigned
    , maybeUnipolar
    , maybeNegative
    , maybePositive

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
    ( Sign
    )
import Internal.Data.Sign.Num
    ( NumSign (..)
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
    , lookup
    , map
    , null
    )

empty :: SignedSet a
empty = CountMap.empty

singleton :: Ord a => a -> SignedSet a
singleton = CountMap.singleton

fromListWith
    :: Ord a
    => (NumSign -> NumSign -> NumSign)
    -> [(a, NumSign)]
    -> SignedSet a
fromListWith = CountMap.fromListWith

fromMap :: Map a NumSign -> SignedSet a
fromMap = CountMap.fromMap

fromSet :: (a -> NumSign) -> Set a -> SignedSet a
fromSet = CountMap.fromSet

toList :: SignedSet a -> [(a, NumSign)]
toList = CountMap.toList

toMap :: SignedSet a -> Map a NumSign
toMap = CountMap.toMap

toSet :: SignedSet a -> Set a
toSet = CountMap.toSet

null :: SignedSet a -> Bool
null = CountMap.null

lookup :: Ord a => a -> SignedSet a -> NumSign
lookup = CountMap.lookup

member :: Ord a => a -> SignedSet a -> Bool
member = CountMap.member

isRegular :: Ord a => SignedSet a -> Bool
isRegular = CountMap.isRegular

isSimple :: Ord a => SignedSet a -> Bool
isSimple = CountMap.isSimple

isSingleton :: SignedSet a -> Bool
isSingleton = CountMap.isSingleton

isSingletonSigned :: SignedSet a -> Bool
isSingletonSigned = CountMap.isSingletonSigned

isUnipolar :: SignedSet a -> Bool
isUnipolar = CountMap.isUnipolar

isBipolar :: SignedSet a -> Bool
isBipolar = CountMap.isBipolar

isNegative :: Ord a => SignedSet a -> Bool
isNegative = CountMap.isNegative

isPositive :: Ord a => SignedSet a -> Bool
isPositive = CountMap.isPositive

maybeRegular :: Ord a => SignedSet a -> Maybe (NumSign, Set a)
maybeRegular = CountMap.maybeRegular

maybeSimple :: Ord a => SignedSet a -> Maybe (NumSign, a)
maybeSimple = CountMap.maybeSimple

maybeSingleton :: Ord a => SignedSet a -> Maybe a
maybeSingleton = CountMap.maybeSingleton

maybeSingletonSigned :: Ord a => SignedSet a -> Maybe (Sign, a)
maybeSingletonSigned = CountMap.maybeSingletonSigned

maybeUnipolar :: SignedSet a -> Maybe (Sign, Set a)
maybeUnipolar = CountMap.maybeUnipolar

maybeNegative :: Ord a => SignedSet a -> Maybe (Set a)
maybeNegative = CountMap.maybeNegative

maybePositive :: Ord a => SignedSet a -> Maybe (Set a)
maybePositive = CountMap.maybePositive

foldl :: (r -> a -> NumSign -> r) -> r -> SignedSet a -> r
foldl = CountMap.foldl

foldl' :: (r -> a -> NumSign -> r) -> r -> SignedSet a -> r
foldl' = CountMap.foldl'

foldr :: (a -> NumSign -> r -> r) -> r -> SignedSet a -> r
foldr = CountMap.foldr

foldr' :: (a -> NumSign -> r -> r) -> r -> SignedSet a -> r
foldr' = CountMap.foldr'

foldMap :: Monoid m => (a -> NumSign -> m) -> SignedSet a -> m
foldMap = CountMap.foldMap

foldMap' :: Monoid m => (a -> NumSign -> m) -> SignedSet a -> m
foldMap' = CountMap.foldMap'

mapWith
    :: Ord b
    => (NumSign -> NumSign -> NumSign)
    -> (a -> b)
    -> SignedSet a
    -> SignedSet b
mapWith = CountMap.mapWith

mapSigns :: (NumSign -> NumSign) -> SignedSet a -> SignedSet a
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
