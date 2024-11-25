{-# LANGUAGE UndecidableInstances #-}

module Data.Multiset.Signed where

import Control.Applicative (Const (Const, getConst))
import Data.Coerce
    ( coerce
    )
import Data.Foldable qualified as Foldable
    ( Foldable (foldl')
    , all
    )
import Data.Foldable1 (Foldable1)
import Data.Foldable1 qualified as Foldable1
import Data.List
    ( partition
    )
import Data.Map.Merge.Strict qualified as Map
import Data.Monoid (All (getAll), Sum (Sum, getSum))
import Data.Monoid qualified
    ( Sum (Sum, getSum)
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Multiset
    ( Multiset
    )
import Data.Multiset qualified as Multiset
import Data.Semigroup (All (All))
import Debug.Trace (trace)
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
    :: Foldable1 f => Ord a => f (SignedMultiset a) -> SignedMultiset a
intersections = Foldable1.foldl1' intersection

testAlignA :: SignedMultiset Char
testAlignA = fromListWith (+) [('a', -1), ('b', 0), ('c', 1)]

testAlignB :: SignedMultiset Char
testAlignB = fromListWith (+) [('b', -1), ('c', 0), ('d', 1)]

align
    :: Ord a
    => SignedMultiset a
    -> SignedMultiset a
    -> [(a, (Integer, Integer))]
align s1 s2 = go (toList s1) (toList s2)
  where
    go [] [] = []
    go ((k1, v1) : kvs1) [] = (k1, (v1, 0)) : go kvs1 []
    go [] ((k2, v2) : kvs2) = (k2, (0, v2)) : go [] kvs2
    go ((k1, v1) : kvs1) ((k2, v2) : kvs2)
        | k1 < k2 = (k1, (v1, 0)) : go kvs1 ((k2, v2) : kvs2)
        | k1 > k2 = (k2, (0, v2)) : go ((k1, v1) : kvs1) kvs2
        | otherwise = (k1, (v1, v2)) : go kvs1 kvs2

-- Caution: this function will only shortcircuit if the sets are incomparable.
compare :: Ord a => SignedMultiset a -> SignedMultiset a -> Maybe Ordering
compare s1 s2 = go False False (compareAll s1 s2)
  where
    go True True _ = Nothing
    go True False [] = Just LT
    go False True [] = Just GT
    go False False [] = Just EQ
    go _seenLT seenGT ((_, LT) : kcs) = go True seenGT kcs
    go seenLT _seenGT ((_, GT) : kcs) = go seenLT True kcs
    go seenLT seenGT ((_, EQ) : kcs) = go seenLT seenGT kcs

compareAll :: Ord a => SignedMultiset a -> SignedMultiset a -> [(a, Ordering)]
compareAll s1 s2 = fmap (uncurry Prelude.compare) <$> align s1 s2

-- Note this will terminate early if a GT is detected.
isLessThan :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isLessThan s1 s2 = go False (compareAll s1 s2)
  where
    go seenLT [] = seenLT
    go seenLT ((_, EQ) : kcs) = go seenLT kcs
    go _ ((_, LT) : kcs) = go True kcs
    go _ ((_, GT) : _) = False

isLessThanOrEqualTo :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isLessThanOrEqualTo s1 s2 = GT `notElem` (snd <$> compareAll s1 s2)

isLessThanOrEqualToU :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isLessThanOrEqualToU (SignedMultiset s1) (SignedMultiset s2) =
    coerce $
        Foldable.foldl' (<>) (All True) $
            MonoidMap.unionWith (\v1 v2 -> All (v1 <= v2)) s1 s2

isSubsetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isSubsetOf (SignedMultiset s1) (SignedMultiset s2) =
    -- actually, can you do this with Map.isSubsetOf?
    -- However, you might need this for properSubset.
    -- And you'll probably need it for isLessThanOrEqualTo.
    go True (MonoidMap.toList s1) (MonoidMap.toList s2)
  where
    go False _ _ = False
    go _ [] _ = True
    go _ (_ : _) [] = False
    go _ ((k1, v1) : kvs1) ((k2, v2) : kvs2)
        | k1 < k2 = False
        | k1 > k2 = go True kvs1 ((k2, v2) : kvs2)
        | otherwise = go (check v1 v2) kvs1 kvs2
    check v1 v2
        | v1 <= 0 && v2 <= 0 = v1 >= v2
        | v1 >= 0 && v2 >= 0 = v1 <= v2
        | otherwise = False

isSubsetOfW :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isSubsetOfW (SignedMultiset s1) (SignedMultiset s2) =
    MonoidMap.isSubmapOfBy check s1 s2
  where
    check v1 v2
        | v1 <= 0 && v2 <= 0 = v1 >= v2
        | v1 >= 0 && v2 >= 0 = v1 <= v2
        | otherwise = False

isSubsetOfM :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isSubsetOfM s1 s2 =
    (n1 `Multiset.isSubsetOf` n2) && (p1 `Multiset.isSubsetOf` p2)
  where
    (n1, p1) = toUnsignedPair s1
    (n2, p2) = toUnsignedPair s2

isProperSubsetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isProperSubsetOf s1 s2 =
    (s1 /= s2) && (s1 `isSubsetOf` s2)

powerset :: Ord a => SignedMultiset a -> [SignedMultiset a]
powerset as =
    [ fromUnsignedPairWith (+) (n, p)
    | n <- Multiset.powerset ns
    , p <- Multiset.powerset ps
    ]
  where
    (ns, ps) = toUnsignedPair as

powersetSize :: Ord a => SignedMultiset a -> Natural
powersetSize (SignedMultiset s) =
    getSum $
        Foldable.foldl' (\x y -> x * (coerce integerMagnitude y + 1)) (Sum 1) s

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

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
