module Data.Set.Signed
    ( -- * Type
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
    , signs

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

      -- * Arithmetic
    , add
    , addMany
    , multiply
    , multiplyMany1

      -- * Extrema
    , union
    , unionMany1
    , intersection
    , intersectionMany1

      -- * Extrema (Supportive)
    , supportiveUnion
    , supportiveUnionMany1
    , supportiveIntersection
    , supportiveIntersectionMany1

      -- * Comparison
    , compareLexically

      -- * Inclusion
    , isSubsetOf
    , isSupersetOf
    , isProperSubsetOf
    , isProperSupersetOf

      -- * Inclusion (Supportive)
    , isSupportiveSubsetOf
    , isSupportiveSupersetOf
    , isProperSupportiveSubsetOf
    , isProperSupportiveSupersetOf

      -- * Inclusion (Symmetric)
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
import Data.Map.Strict qualified as Map
import Data.Maybe
    ( mapMaybe
    )
import Data.Set
    ( Set
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Data.Sign
    ( Sign
    )
import Internal.Data.Sign qualified as Sign
import Internal.Data.Sign.Num
    ( NumSign
    )
import Internal.Data.Sign.Num qualified as NumSign
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
    , max
    , min
    , null
    )

empty :: SignedSet a
empty = CountMap.empty

singleton :: Ord a => a -> SignedSet a
singleton = CountMap.singleton

fromListWith :: Ord a => (Sign -> Sign -> Sign) -> [(a, Sign)] -> SignedSet a
fromListWith f =
    CountMap.fromListWith (unsafeSignToNumSign3 f)
        . fmap (fmap NumSign.fromSign)

fromMap :: Map a Sign -> SignedSet a
fromMap = CountMap.fromMap . Map.map NumSign.fromSign

fromSet :: (a -> Sign) -> Set a -> SignedSet a
fromSet = CountMap.fromSet . fmap NumSign.fromSign

toList :: SignedSet a -> [(a, Sign)]
toList = mapMaybe (traverse NumSign.toSign) . CountMap.toList

toMap :: SignedSet a -> Map a Sign
toMap = Map.mapMaybe NumSign.toSign . CountMap.toMap

toSet :: SignedSet a -> Set a
toSet = CountMap.toSet

null :: SignedSet a -> Bool
null = CountMap.null

lookup :: Ord a => a -> SignedSet a -> Maybe Sign
lookup a = NumSign.toSign . CountMap.lookup a

member :: Ord a => a -> SignedSet a -> Bool
member = CountMap.member

signs :: SignedSet a -> Set Sign
signs = CountMap.signs

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

maybeRegular :: Ord a => SignedSet a -> Maybe (Sign, Set a)
maybeRegular s = do
    (numSign, regularSet) <- CountMap.maybeRegular s
    sign <- NumSign.toSign numSign
    pure (sign, regularSet)

maybeSimple :: Ord a => SignedSet a -> Maybe (Sign, a)
maybeSimple s = do
    (numSign, a) <- CountMap.maybeSimple s
    sign <- NumSign.toSign numSign
    pure (sign, a)

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

foldl :: (r -> a -> Sign -> r) -> r -> SignedSet a -> r
foldl f = CountMap.foldl (\r a -> f r a . unsafeNumSignToSign)

foldl' :: (r -> a -> Sign -> r) -> r -> SignedSet a -> r
foldl' f = CountMap.foldl' (\r a -> f r a . unsafeNumSignToSign)

foldr :: (a -> Sign -> r -> r) -> r -> SignedSet a -> r
foldr f = CountMap.foldr (\a -> f a . unsafeNumSignToSign)

foldr' :: (a -> Sign -> r -> r) -> r -> SignedSet a -> r
foldr' f = CountMap.foldr' (\a -> f a . unsafeNumSignToSign)

foldMap :: Monoid m => (a -> Sign -> m) -> SignedSet a -> m
foldMap f = CountMap.foldMap (\a -> f a . unsafeNumSignToSign)

foldMap' :: Monoid m => (a -> Sign -> m) -> SignedSet a -> m
foldMap' f = CountMap.foldMap' (\a -> f a . unsafeNumSignToSign)

mapWith
    :: Ord b
    => (Sign -> Sign -> Sign)
    -> (a -> b)
    -> SignedSet a
    -> SignedSet b
mapWith = CountMap.mapWith . unsafeSignToNumSign3

mapSigns :: (Sign -> Sign) -> SignedSet a -> SignedSet a
mapSigns = CountMap.mapCounts . unsafeSignToNumSign2

invert :: SignedSet a -> SignedSet a
invert = CountMap.invert

add :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
add = CountMap.add

addMany :: Foldable f => Ord a => f (SignedSet a) -> SignedSet a
addMany = CountMap.addMany

multiply :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
multiply = CountMap.multiply

multiplyMany1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
multiplyMany1 = CountMap.multiplyMany1

union :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
union = CountMap.union

unionMany1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
unionMany1 = CountMap.unionMany1

intersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
intersection = CountMap.intersection

intersectionMany1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
intersectionMany1 = CountMap.intersectionMany1

supportiveUnion :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
supportiveUnion = CountMap.supportiveUnion

supportiveUnionMany1 :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
supportiveUnionMany1 = CountMap.supportiveUnionMany1

supportiveIntersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
supportiveIntersection = CountMap.supportiveIntersection

supportiveIntersectionMany1
    :: Foldable1 f
    => Ord a
    => f (SignedSet a)
    -> SignedSet a
supportiveIntersectionMany1 = CountMap.supportiveIntersectionMany1

compareLexically :: Ord a => SignedSet a -> SignedSet a -> Ordering
compareLexically = CountMap.compareLexically

isSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSubsetOf = CountMap.isSubmapOf

isSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSupersetOf = CountMap.isSupermapOf

isProperSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSubsetOf = CountMap.isProperSubmapOf

isProperSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSupersetOf = CountMap.isProperSupermapOf

isSupportiveSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSupportiveSubsetOf = CountMap.isSupportiveSubmapOf

isSupportiveSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isSupportiveSupersetOf = CountMap.isSupportiveSupermapOf

isProperSupportiveSubsetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSupportiveSubsetOf = CountMap.isProperSupportiveSubmapOf

isProperSupportiveSupersetOf :: Ord a => SignedSet a -> SignedSet a -> Bool
isProperSupportiveSupersetOf = CountMap.isProperSupportiveSupermapOf

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

unsafeNumSignToSign :: NumSign -> Sign
unsafeNumSignToSign = \case
    NumSign.N -> Sign.N
    NumSign.P -> Sign.P
    NumSign.Z -> error "unsafeNumSignToSign"

unsafeSignToNumSign2
    :: (Sign -> Sign) -> (NumSign -> NumSign)
unsafeSignToNumSign2 f ns =
    NumSign.fromSign $
        f (unsafeNumSignToSign ns)

unsafeSignToNumSign3
    :: (Sign -> Sign -> Sign) -> (NumSign -> NumSign -> NumSign)
unsafeSignToNumSign3 f ns1 ns2 =
    NumSign.fromSign $
        f
            (unsafeNumSignToSign ns1)
            (unsafeNumSignToSign ns2)
