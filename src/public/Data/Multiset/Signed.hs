{-# LANGUAGE UndecidableInstances #-}

module Data.Multiset.Signed where

import Data.Coerce
    ( coerce
    )
import Data.Foldable qualified as Foldable
    ( Foldable (foldl')
    )
import Data.Foldable1 (Foldable1)
import Data.Foldable1 qualified as Foldable1
import Data.Function (on)
import Data.List
    ( partition
    )
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (Sum, getSum))
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Multiset
    ( Multiset
    )
import Data.Multiset qualified as Multiset
import Data.Set qualified as Set
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( compare
    , sum
    )
import Prelude qualified

newtype SignedMultiset a
    = SignedMultiset (MonoidMap a (Data.Monoid.Sum Integer))
    deriving newtype (Eq)

instance Show a => Show (SignedMultiset a) where
    show s = "fromListWith (+) " <> show (toList s)

testA :: SignedMultiset Char
testA = fromListWith (+) [('a', -1), ('b', 1), ('c', 0), ('d', -2), ('e', 5)]

testB :: SignedMultiset Char
testB = fromListWith (+) [('a', -2), ('b', 1), ('c', 0), ('d', -2), ('e', 5)]

fromListWith
    :: Ord a
    => (Integer -> Integer -> Integer)
    -> [(a, Integer)]
    -> SignedMultiset a
fromListWith f =
    SignedMultiset
        . MonoidMap.fromListWith (coerce f)
        . fmap (fmap Data.Monoid.Sum)

empty :: SignedMultiset a
empty = SignedMultiset MonoidMap.empty

toList :: SignedMultiset a -> [(a, Integer)]
toList (SignedMultiset s) = coerce (MonoidMap.toList s)

toUnsignedPair :: Ord a => SignedMultiset a -> (Multiset a, Multiset a)
toUnsignedPair m =
    ( Multiset.fromListWith (+) $
        fmap (fmap integerNegativePartToNatural) ns
    , Multiset.fromListWith (+) $
        fmap (fmap integerPositivePartToNatural) ps
    )
  where
    (ns, ps) = partition ((< 0) . snd) (toList m)

invert :: SignedMultiset a -> SignedMultiset a
invert (SignedMultiset s) = SignedMultiset (MonoidMap.invert s)

negativePart :: Ord a => SignedMultiset a -> Multiset a
negativePart m =
    Multiset.fromListWith (+) $
        fmap integerNegativePartToNatural
            <$> filter ((< 0) . snd) (toList m)

positivePart :: Ord a => SignedMultiset a -> Multiset a
positivePart m =
    Multiset.fromListWith (+) $
        fmap integerPositivePartToNatural
            <$> filter ((> 0) . snd) (toList m)

-- cardinalitySum
-- cardinalityAbs
-- cardinalityPositive
-- cardinalityNegative

cardinality :: SignedMultiset a -> Integer
cardinality (SignedMultiset s) =
    Data.Monoid.getSum $ Foldable.foldl' (+) 0 s

magnitude :: SignedMultiset a -> Natural
magnitude (SignedMultiset s) =
    Data.Monoid.getSum $ foldMap (coerce integerMagnitude) s

fromUnsignedNegative :: Multiset a -> SignedMultiset a
fromUnsignedNegative = undefined

fromUnsignedPositive :: Multiset a -> SignedMultiset a
fromUnsignedPositive = undefined

fromUnsignedPairWith
    :: Ord a
    => (Integer -> Integer -> Integer)
    -> (Multiset a, Multiset a)
    -> SignedMultiset a
fromUnsignedPairWith f (s1, s2) =
    fromListWith f (ns <> ps)
  where
    ns = fmap naturalToNegativeInteger <$> Multiset.toList s1
    ps = fmap naturalToPositiveInteger <$> Multiset.toList s2

difference
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
difference (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ m1 `MonoidMap.minus` m2

sum
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
sum (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.unionWith (+) m1 m2

sums
    :: Foldable f
    => Ord a
    => f (SignedMultiset a)
    -> SignedMultiset a
sums = Foldable.foldl' sum empty

union
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
union (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.unionWith max m1 m2

unions
    :: Foldable f
    => Ord a
    => f (SignedMultiset a)
    -> SignedMultiset a
unions = Foldable.foldl' union empty

intersection
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> SignedMultiset a
intersection (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.unionWith min m1 m2

intersections
    :: Foldable1 f
    => Ord a
    => f (SignedMultiset a) -> SignedMultiset a
intersections = Foldable1.foldl1' intersection

-- Caution: this function will only short-circuit if the sets are incomparable.
--
{- ORMOLU_DISABLE -}
compare :: Ord a => SignedMultiset a -> SignedMultiset a -> Maybe Ordering
compare s1 s2 = go False False (compareAll s1 s2)
  where
    go    True    True              _ = Nothing
    go    True   False             [] = Just LT
    go   False    True             [] = Just GT
    go   False   False             [] = Just EQ
    go _seenLT  seenGT ((_, LT) : xs) = go True   seenGT xs
    go  seenLT _seenGT ((_, GT) : xs) = go seenLT   True xs
    go  seenLT  seenGT ((_, EQ) : xs) = go seenLT seenGT xs
{- ORMOLU_ENABLE -}

compareAll :: Ord a => SignedMultiset a -> SignedMultiset a -> [(a, Ordering)]
compareAll s1 s2 = fmap (uncurry Prelude.compare) <$> align s1 s2

-- Note this will terminate early if a GT is detected.
{- ORMOLU_DISABLE -}
isLessThan :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isLessThan s1 s2 = go False (compareAll s1 s2)
  where
    go seenLT             [] = seenLT
    go _      ((_, LT) : xs) = go True   xs
    go seenLT ((_, EQ) : xs) = go seenLT xs
    go _      ((_, GT) :  _) = False
{- ORMOLU_ENABLE -}

-- Note this will terminate early if a GT is detected.
{- ORMOLU_DISABLE -}
isGreaterThan :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isGreaterThan s1 s2 = go False (compareAll s1 s2)
  where
    go seenGT             [] = seenGT
    go _      ((_, LT) :  _) = False
    go seenGT ((_, EQ) : xs) = go seenGT xs
    go _      ((_, GT) : xs) = go True   xs
{- ORMOLU_ENABLE -}

isLessThanOrEqualTo :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isLessThanOrEqualTo s1 s2 = GT `notElem` (snd <$> compareAll s1 s2)

isGreaterThanOrEqualTo :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isGreaterThanOrEqualTo s1 s2 = LT `notElem` (snd <$> compareAll s1 s2)

isSubsetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isSubsetOf (SignedMultiset s1) (SignedMultiset s2) =
    (Map.isSubmapOfBy isContainedBy `on` MonoidMap.toMap) s1 s2

isSupersetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isSupersetOf = flip isSubsetOf

isProperSubsetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isProperSubsetOf (SignedMultiset s1) (SignedMultiset s2) =
    (Map.isProperSubmapOfBy isContainedBy `on` MonoidMap.toMap) s1 s2

isProperSupersetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isProperSupersetOf = flip isProperSubsetOf

powersetSize :: Ord a => SignedMultiset a -> Natural
powersetSize (SignedMultiset s) =
    getSum $
        Foldable.foldl'
            (\x y -> x * (coerce integerMagnitude y + 1))
            1
            s

--------------------------------------------------------------------------------
-- Model functions
--------------------------------------------------------------------------------

modelIsLessThan
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> Bool
modelIsLessThan s1 s2 =
    (s1 /= s2) && (s1 `modelIsLessThanOrEqualTo` s2)

modelIsGreaterThan
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> Bool
modelIsGreaterThan s1 s2 =
    (s1 /= s2) && (s1 `modelIsGreaterThanOrEqualTo` s2)

modelIsLessThanOrEqualTo
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> Bool
modelIsLessThanOrEqualTo (SignedMultiset s1) (SignedMultiset s2) =
    all
        (\k -> ((<=) `on` MonoidMap.get k) s1 s2)
        ((Set.union `on` MonoidMap.nonNullKeys) s1 s2)

modelIsGreaterThanOrEqualTo
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> Bool
modelIsGreaterThanOrEqualTo (SignedMultiset s1) (SignedMultiset s2) =
    all
        (\k -> ((>=) `on` MonoidMap.get k) s1 s2)
        ((Set.union `on` MonoidMap.nonNullKeys) s1 s2)

modelIsSubsetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
modelIsSubsetOf (SignedMultiset s1) (SignedMultiset s2) =
    all
        (\k -> (isContainedBy `on` MonoidMap.get k) s1 s2)
        ((Set.union `on` MonoidMap.nonNullKeys) s1 s2)

modelIsSupersetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
modelIsSupersetOf = flip modelIsSubsetOf

modelIsProperSubsetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
modelIsProperSubsetOf s1 s2 = (s1 /= s2) && modelIsSubsetOf s1 s2

modelIsProperSupersetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
modelIsProperSupersetOf = flip modelIsProperSubsetOf

modelPowerset :: Ord a => SignedMultiset a -> [SignedMultiset a]
modelPowerset as =
    [ fromUnsignedPairWith (+) (n, p)
    | n <- Multiset.powerset ns
    , p <- Multiset.powerset ps
    ]
  where
    (ns, ps) = toUnsignedPair as

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

testAlignA :: SignedMultiset Char
testAlignA = fromListWith (+) [('a', -1), ('b', 0), ('c', 1)]

testAlignB :: SignedMultiset Char
testAlignB = fromListWith (+) [('b', -1), ('c', 0), ('d', 1)]

{- ORMOLU_DISABLE -}
align
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> [(a, (Integer, Integer))]
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

integerMagnitude :: Integer -> Natural
integerMagnitude n = fromIntegral (abs n)

integerToNaturalPair :: Integer -> (Natural, Natural)
integerToNaturalPair n
    | n < 0 = (fromIntegral (abs n), 0)
    | otherwise = (0, fromIntegral n)

integerNegativePartToNatural :: Integer -> Natural
integerNegativePartToNatural n
    | n < 0 = fromIntegral (abs n)
    | otherwise = 0

integerPositivePartToNatural :: Integer -> Natural
integerPositivePartToNatural n
    | n > 0 = fromIntegral n
    | otherwise = 0

naturalToNegativeInteger :: Natural -> Integer
naturalToNegativeInteger = negate . fromIntegral

naturalToPositiveInteger :: Natural -> Integer
naturalToPositiveInteger = fromIntegral

isContainedBy :: (Ord a, Num a) => a -> a -> Bool
isContainedBy v1 v2
    | v1 <= 0 && v2 <= 0 = v1 >= v2
    | v1 >= 0 && v2 >= 0 = v1 <= v2
    | otherwise = False
