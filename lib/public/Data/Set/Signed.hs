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
import Data.Function
    ( on
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
    , null
    )

empty :: SignedSet a
empty = CountMap.empty

singleton :: Ord a => a -> SignedSet a
singleton = CountMap.singleton

fromListWith :: Ord a => (Sign -> Sign -> Sign) -> [(a, Sign)] -> SignedSet a
fromListWith f as = CountMap.fromListWith g bs
  where
    g = unsafeSignToNumSign3 f
    bs = fmap NumSign.fromSign <$> as

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
    => (Sign -> Sign -> Sign)
    -> (a -> b)
    -> SignedSet a
    -> SignedSet b
mapWith = CountMap.mapWith . unsafeSignToNumSign3

mapSigns :: (Sign -> Sign) -> SignedSet a -> SignedSet a
mapSigns = CountMap.mapCounts . unsafeSignToNumSign2

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

unsafeNumSignToSign :: NumSign -> Sign
unsafeNumSignToSign = \case
    NumSign.N -> Sign.N
    NumSign.P -> Sign.P
    NumSign.Z -> error "unsafeNumSignToSign"

{- ORMOLU_DISABLE -}
unsafeSignToNumSign2
    :: (   Sign ->    Sign)
    -> (NumSign -> NumSign)
unsafeSignToNumSign2 f = NumSign.fromSign . f . unsafeNumSignToSign
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
unsafeSignToNumSign3
    :: (   Sign ->    Sign ->    Sign)
    -> (NumSign -> NumSign -> NumSign)
unsafeSignToNumSign3 f = fmap NumSign.fromSign <$> f `on` unsafeNumSignToSign
{- ORMOLU_ENABLE -}
