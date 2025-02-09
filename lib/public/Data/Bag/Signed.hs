module Data.Bag.Signed
    ( -- * Type
      SignedBag

      -- * Construction
    , empty
    , singleton
    , fromList
    , fromListWith
    , fromMap
    , fromSet

      -- * Deconstruction
    , toList
    , toMap
    , toSet
    , toSetSigned

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
    , map
    , mapWith
    , mapCounts

      -- * Transformation
    , invert

      -- * Arithmetic
    , add
    , addMany
    , multiply
    , multiplyMany1

      -- * Bounds
    , union
    , unionMany1
    , intersection
    , intersectionMany1

      -- * Bounds (Supportive)
    , supportiveUnion
    , supportiveUnionMany1
    , supportiveIntersection
    , supportiveIntersectionMany1

      -- * Difference
    , difference
    , symmetricDifference
    , symmetricDifferenceUnsigned

      -- * Comparison
    , compareLexically

      -- * Inclusion
    , isSubbagOf
    , isSuperbagOf
    , isProperSubbagOf
    , isProperSuperbagOf

      -- * Inclusion (Supportive)
    , isSupportiveSubbagOf
    , isSupportiveSuperbagOf
    , isProperSupportiveSubbagOf
    , isProperSupportiveSuperbagOf

      -- * Inclusion (Symmetric)
    , isSymmetricSubbagOf
    , isSymmetricSuperbagOf
    , isProperSymmetricSubbagOf
    , isProperSymmetricSuperbagOf

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
import Internal.Shared
    ( Bag
    , SignedBag
    , SignedSet
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
    , sum
    )

empty :: SignedBag a
empty = CountMap.empty

singleton :: Ord a => a -> SignedBag a
singleton = CountMap.singleton

fromList :: Ord a => [(a, Integer)] -> SignedBag a
fromList = CountMap.fromList

fromListWith
    :: Ord a
    => (Integer -> Integer -> Integer)
    -> [(a, Integer)]
    -> SignedBag a
fromListWith = CountMap.fromListWith

fromMap :: Map a Integer -> SignedBag a
fromMap = CountMap.fromMap

fromSet :: (a -> Integer) -> Set a -> SignedBag a
fromSet = CountMap.fromSet

toList :: SignedBag a -> [(a, Integer)]
toList = CountMap.toList

toMap :: SignedBag a -> Map a Integer
toMap = CountMap.toMap

toSet :: SignedBag a -> Set a
toSet = CountMap.toSet

toSetSigned :: SignedBag a -> SignedSet a
toSetSigned = CountMap.toSetSigned

null :: SignedBag a -> Bool
null = CountMap.null

lookup :: Ord a => a -> SignedBag a -> Integer
lookup = CountMap.lookup

member :: Ord a => a -> SignedBag a -> Bool
member = CountMap.member

signs :: SignedBag a -> Set Sign
signs = CountMap.signs

isRegular :: Ord a => SignedBag a -> Bool
isRegular = CountMap.isRegular

isSimple :: Ord a => SignedBag a -> Bool
isSimple = CountMap.isSimple

isSingleton :: SignedBag a -> Bool
isSingleton = CountMap.isSingleton

isSingletonSigned :: SignedBag a -> Bool
isSingletonSigned = CountMap.isSingletonSigned

isUnipolar :: SignedBag a -> Bool
isUnipolar = CountMap.isUnipolar

isBipolar :: SignedBag a -> Bool
isBipolar = CountMap.isBipolar

isNegative :: Ord a => SignedBag a -> Bool
isNegative = CountMap.isNegative

isPositive :: Ord a => SignedBag a -> Bool
isPositive = CountMap.isPositive

maybeRegular :: Ord a => SignedBag a -> Maybe (Integer, Set a)
maybeRegular = CountMap.maybeRegular

maybeSimple :: Ord a => SignedBag a -> Maybe (Integer, a)
maybeSimple = CountMap.maybeSimple

maybeSingleton :: Ord a => SignedBag a -> Maybe a
maybeSingleton = CountMap.maybeSingleton

maybeSingletonSigned :: Ord a => SignedBag a -> Maybe (Sign, a)
maybeSingletonSigned = CountMap.maybeSingletonSigned

maybeUnipolar :: SignedBag a -> Maybe (Sign, Bag a)
maybeUnipolar = CountMap.maybeUnipolar

maybeNegative :: Ord a => SignedBag a -> Maybe (Bag a)
maybeNegative = CountMap.maybeNegative

maybePositive :: Ord a => SignedBag a -> Maybe (Bag a)
maybePositive = CountMap.maybePositive

foldl :: (r -> a -> Integer -> r) -> r -> SignedBag a -> r
foldl = CountMap.foldl

foldl' :: (r -> a -> Integer -> r) -> r -> SignedBag a -> r
foldl' = CountMap.foldl'

foldr :: (a -> Integer -> r -> r) -> r -> SignedBag a -> r
foldr = CountMap.foldr

foldr' :: (a -> Integer -> r -> r) -> r -> SignedBag a -> r
foldr' = CountMap.foldr'

foldMap :: Monoid m => (a -> Integer -> m) -> SignedBag a -> m
foldMap = CountMap.foldMap

foldMap' :: Monoid m => (a -> Integer -> m) -> SignedBag a -> m
foldMap' = CountMap.foldMap'

map :: Ord b => (a -> b) -> SignedBag a -> SignedBag b
map = CountMap.map

mapWith
    :: Ord b
    => (Integer -> Integer -> Integer)
    -> (a -> b)
    -> SignedBag a
    -> SignedBag b
mapWith = CountMap.mapWith

mapCounts :: (Integer -> Integer) -> SignedBag a -> SignedBag a
mapCounts = CountMap.mapCounts

invert :: SignedBag a -> SignedBag a
invert = CountMap.invert

add :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
add = CountMap.add

addMany :: Foldable f => Ord a => f (SignedBag a) -> SignedBag a
addMany = CountMap.addMany

multiply :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
multiply = CountMap.multiply

multiplyMany1 :: Foldable1 f => Ord a => f (SignedBag a) -> SignedBag a
multiplyMany1 = CountMap.multiplyMany1

union :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
union = CountMap.union

unionMany1 :: Foldable1 f => Ord a => f (SignedBag a) -> SignedBag a
unionMany1 = CountMap.unionMany1

intersection :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
intersection = CountMap.intersection

intersectionMany1 :: Foldable1 f => Ord a => f (SignedBag a) -> SignedBag a
intersectionMany1 = CountMap.intersectionMany1

supportiveUnion :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
supportiveUnion = CountMap.supportiveUnion

supportiveUnionMany1 :: Foldable1 f => Ord a => f (SignedBag a) -> SignedBag a
supportiveUnionMany1 = CountMap.supportiveUnionMany1

supportiveIntersection :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
supportiveIntersection = CountMap.supportiveIntersection

supportiveIntersectionMany1
    :: Foldable1 f
    => Ord a
    => f (SignedBag a)
    -> SignedBag a
supportiveIntersectionMany1 = CountMap.supportiveIntersectionMany1

difference :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
difference = CountMap.minus

symmetricDifference :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
symmetricDifference = CountMap.symmetricDifference

symmetricDifferenceUnsigned :: Ord a => SignedBag a -> SignedBag a -> Bag a
symmetricDifferenceUnsigned = CountMap.symmetricDifferenceAbsolute

compareLexically :: Ord a => SignedBag a -> SignedBag a -> Ordering
compareLexically = CountMap.compareLexically

isSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSubbagOf = CountMap.isSubmapOf

isSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSuperbagOf = CountMap.isSupermapOf

isProperSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSubbagOf = CountMap.isProperSubmapOf

isProperSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSuperbagOf = CountMap.isProperSupermapOf

isSupportiveSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSupportiveSubbagOf = CountMap.isSupportiveSubmapOf

isSupportiveSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSupportiveSuperbagOf = CountMap.isSupportiveSupermapOf

isProperSupportiveSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSupportiveSubbagOf = CountMap.isProperSupportiveSubmapOf

isProperSupportiveSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSupportiveSuperbagOf = CountMap.isProperSupportiveSupermapOf

isSymmetricSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSymmetricSubbagOf = CountMap.isSymmetricSubmapOf

isSymmetricSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSymmetricSuperbagOf = CountMap.isSymmetricSupermapOf

isProperSymmetricSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSymmetricSubbagOf = CountMap.isProperSymmetricSubmapOf

isProperSymmetricSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSymmetricSuperbagOf = CountMap.isProperSymmetricSupermapOf

symmetricPowerset :: Ord a => SignedBag a -> Set (SignedBag a)
symmetricPowerset = CountMap.symmetricPowerset

symmetricPowersetElements :: Ord a => SignedBag a -> [SignedBag a]
symmetricPowersetElements = CountMap.symmetricPowersetElements

symmetricPowersetSize :: Ord a => SignedBag a -> Natural
symmetricPowersetSize = CountMap.symmetricPowersetSize

{-

fromSetPositive :: Set a -> SignedBag a
fromSetPositive = fromSetWith (const 1)

fromSetNegative :: Set a -> SignedBag a
fromSetNegative = fromSetWith (const (-1))

fromSetWith :: (a -> I) -> Set a -> SignedBag a
fromSetWith f = fromMap . Map.fromSet (coerce f)

toUnsignedPair :: Ord a => SignedBag a -> (Bag a, Bag a)
toUnsignedPair m =
    ( Bag.fromListWith (+) $
        fmap (fmap integerNegativePartToNatural) ns
    , Bag.fromListWith (+) $
        fmap (fmap integerPositivePartToNatural) ps
    )
  where
    (ns, ps) = partition ((< 0) . snd) (toList m)

negativePart :: Ord a => SignedBag a -> Bag a
negativePart m =
    Bag.fromListWith (+) $
        fmap integerNegativePartToNatural
            <$> filter ((< 0) . snd) (toList m)

positivePart :: Ord a => SignedBag a -> Bag a
positivePart m =
    Bag.fromListWith (+) $
        fmap integerPositivePartToNatural
            <$> filter ((> 0) . snd) (toList m)

-- cardinalityCount
-- cardinalityAbsolute
-- cardinalityPositive
-- cardinalityNegative

cardinality :: SignedBag a -> I
cardinality (SignedBag s) =
    getCount $ Foldable.foldl' (+) 0 s

magnitude :: SignedBag a -> Natural
magnitude (SignedBag s) =
    getCount $ foldMap (coerce integerMagnitude) s

fromUnsignedNegative :: Bag a -> SignedBag a
fromUnsignedNegative = undefined

fromUnsignedPositive :: Bag a -> SignedBag a
fromUnsignedPositive = undefined

fromUnsignedPairWith
    :: Ord a
    => (I -> I -> I)
    -> (Bag a, Bag a)
    -> SignedBag a
fromUnsignedPairWith f (s1, s2) =
    fromListWith f (ns <> ps)
  where
    ns = fmap naturalToNegativeI <$> Bag.toList s1
    ps = fmap naturalToPositiveI <$> Bag.toList s2

isPositive :: SignedBag a -> Bool
isPositive = undefined

isNegative :: SignedBag a -> Bool
isNegative = undefined

maybePositive :: SignedBag a -> Maybe (Bag a)
maybePositive = undefined

maybeNegative :: SignedBag a -> Maybe (Bag a)
maybeNegative = undefined

isPositiveSet :: SignedBag a -> Bool
isPositiveSet (SignedBag s) = Foldable.all (== 1) s

isNegativeSet :: SignedBag a -> Bool
isNegativeSet (SignedBag s) = Foldable.all (== (-1)) s

maybePositiveSet :: SignedBag a -> Maybe (Set a)
maybePositiveSet = undefined

maybeNegativeSet :: SignedBag a -> Maybe (Set a)
maybeNegativeSet = undefined

support :: SignedBag a -> Set a
support (SignedBag b) = MonoidMap.nonNullKeys b

supportSigned :: SignedBag a -> SignedSet a
supportSigned (SignedBag b) = undefined --  SignedSet (MonoidMap.map f b)
  where
    f (Count n) = undefined -- Sum (Sign.integralToSign n)

supportPositive :: SignedBag a -> Set a
supportPositive = undefined

supportNegative :: SignedBag a -> Set a
supportNegative = undefined

--------------------------------------------------------------------------------
-- Model functions
--------------------------------------------------------------------------------

model_isSubsetOf
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> Bool
model_isSubsetOf s1 s2 =
    all
        (\k -> ((<=) `on` multiplicity k) s1 s2)
        ((Set.union `on` support) s1 s2)

model_isSupersetOf
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> Bool
model_isSupersetOf s1 s2 =
    all
        (\k -> ((>=) `on` multiplicity k) s1 s2)
        ((Set.union `on` support) s1 s2)

model_isProperSubsetOf
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> Bool
model_isProperSubsetOf s1 s2 =
    (s1 /= s2) && (s1 `model_isSubsetOf` s2)

model_isProperSupersetOf
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> Bool
model_isProperSupersetOf s1 s2 =
    (s1 /= s2) && (s1 `model_isSupersetOf` s2)

model_isSymmetricSubsetOf
    :: Ord a => SignedBag a -> SignedBag a -> Bool
model_isSymmetricSubsetOf s1 s2 =
    all
        (\k -> (isSmallerThan `on` multiplicity k) s1 s2)
        ((Set.union `on` support) s1 s2)

model_isSymmetricSupersetOf
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> Bool
model_isSymmetricSupersetOf = flip model_isSymmetricSubsetOf

model_isProperSymmetricSubsetOf
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> Bool
model_isProperSymmetricSubsetOf s1 s2 =
    (s1 /= s2) && model_isSymmetricSubsetOf s1 s2

model_isProperSymmetricSupersetOf
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> Bool
model_isProperSymmetricSupersetOf = flip model_isProperSymmetricSubsetOf

model_symmetricPowersetElements
    :: Ord a
    => SignedBag a
    -> [SignedBag a]
model_symmetricPowersetElements as =
    [ fromUnsignedPairWith (+) (n, p)
    | n <- Bag.powersetElements ns
    , p <- Bag.powersetElements ps
    ]
  where
    (ns, ps) = toUnsignedPair as

-}
