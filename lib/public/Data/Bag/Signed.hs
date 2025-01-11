module Data.Bag.Signed
    (
    -- * Type
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

    -- * Membership
    , null
    , count
    , member

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

    -- * Combination
    , add
    , addMany
    , union
    , unionMany1
    , intersection
    , intersectionMany1
    , difference
    , symmetricDifference
    , symmetricDifferenceUnsigned

    -- * Ordering

    -- ** Total
    , compareLexically
    , compareColexically

    -- ** Partial
    , isLessThan
    , isLessThanOrEqualTo
    , isGreaterThan
    , isGreaterThanOrEqualTo
    , isSubbagOf
    , isSuperbagOf
    , isProperSubbagOf
    , isProperSuperbagOf
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
import Internal.Shared
    ( SignedBag
    , Bag
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( compare
    , foldl
    , foldl'
    , foldr
    , foldMap
    , map
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

null :: SignedBag a -> Bool
null = CountMap.null

count :: Ord a => a -> SignedBag a -> Integer
count = CountMap.count

member :: Ord a => a -> SignedBag a -> Bool
member = CountMap.member

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

union :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
union = CountMap.union

unionMany1 :: Foldable1 f => Ord a => f (SignedBag a) -> SignedBag a
unionMany1 = CountMap.unionMany1

intersection :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
intersection = CountMap.intersection

intersectionMany1 :: Foldable1 f => Ord a => f (SignedBag a) -> SignedBag a
intersectionMany1 = CountMap.intersectionMany1

difference :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
difference = CountMap.minus

symmetricDifference :: Ord a => SignedBag a -> SignedBag a -> SignedBag a
symmetricDifference = CountMap.symmetricDifference

symmetricDifferenceUnsigned :: Ord a => SignedBag a -> SignedBag a -> Bag a
symmetricDifferenceUnsigned = CountMap.symmetricDifferenceAbsolute

compareLexically :: Ord a => SignedBag a -> SignedBag a -> Ordering
compareLexically = CountMap.compareLexically

compareColexically :: Ord a => SignedBag a -> SignedBag a -> Ordering
compareColexically = CountMap.compareLexically

isLessThan :: Ord a => SignedBag a -> SignedBag a -> Bool
isLessThan = CountMap.isLessThan

isLessThanOrEqualTo :: Ord a => SignedBag a -> SignedBag a -> Bool
isLessThanOrEqualTo = CountMap.isLessThanOrEqualTo

isGreaterThan :: Ord a => SignedBag a -> SignedBag a -> Bool
isGreaterThan = CountMap.isGreaterThan

isGreaterThanOrEqualTo :: Ord a => SignedBag a -> SignedBag a -> Bool
isGreaterThanOrEqualTo = CountMap.isGreaterThanOrEqualTo

isSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSubbagOf = CountMap.isSubmapOf

isSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isSuperbagOf = CountMap.isSupermapOf

isProperSubbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSubbagOf = CountMap.isProperSubmapOf

isProperSuperbagOf :: Ord a => SignedBag a -> SignedBag a -> Bool
isProperSuperbagOf = CountMap.isProperSupermapOf

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

multiplicity :: Ord a => a -> SignedBag a -> I
multiplicity a (SignedBag s) = coerce (MonoidMap.get a s)

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

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

testAlignA :: SignedBag Char
testAlignA = fromListWith (+) [('a', -1), ('b', 0), ('c', 1)]

testAlignB :: SignedBag Char
testAlignB = fromListWith (+) [('b', -1), ('c', 0), ('d', 1)]

{- ORMOLU_DISABLE -}
align
    :: Ord a
    => SignedBag a
    -> SignedBag a
    -> [(a, (I, I))]
align = go `on` toList
  where
    go            []            [] = []
    go ((a, p) : xs)            [] = (a, (p, 0)) : go xs []
    go            [] ((b, q) : ys) = (b, (0, q)) : go [] ys
    go ((a, p) : xs) ((b, q) : ys)
        | a < b                    = (a, (p, 0)) : go           xs ((b, q) : ys)
        | a > b                    = (b, (0, q)) : go ((a, p) : xs)          ys
        | otherwise                = (a, (p, q)) : go           xs           ys
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
integerDistance :: I -> I -> Natural
integerDistance a b
    | a > b     = fromIntegral (a - b)
    | otherwise = fromIntegral (b - a)
{- ORMOLU_ENABLE -}

integerMagnitude :: I -> Natural
integerMagnitude n = fromIntegral (abs n)

integerToNaturalPair :: I -> (Natural, Natural)
integerToNaturalPair n
    | n < 0 = (fromIntegral (abs n), 0)
    | otherwise = (0, fromIntegral n)

integerNegativePartToNatural :: I -> Natural
integerNegativePartToNatural n
    | n < 0 = fromIntegral (abs n)
    | otherwise = 0

integerPositivePartToNatural :: I -> Natural
integerPositivePartToNatural n
    | n > 0 = fromIntegral n
    | otherwise = 0

naturalToNegativeI :: Natural -> I
naturalToNegativeI = negate . fromIntegral

naturalToPositiveI :: Natural -> I
naturalToPositiveI = fromIntegral

isSmallerThan :: (Ord a, Num a) => a -> a -> Bool
isSmallerThan v1 v2
    | v1 <= 0 && v2 <= 0 = v1 >= v2
    | v1 >= 0 && v2 >= 0 = v1 <= v2
    | otherwise = False
-}
