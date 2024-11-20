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
import Data.List
    ( partition
    )
import Data.Monoid (Sum (Sum, getSum))
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
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( sum
    )

newtype SignedMultiset a
    = SignedMultiset (MonoidMap a (Data.Monoid.Sum Integer))
    deriving newtype (Eq)

instance Show a => Show (SignedMultiset a) where
    show s = "fromListWith (+) " <> show (toList s)

testA :: SignedMultiset Char
testA = fromListWith (+) [('a', -1), ('b', 1), ('c', -2), ('d', 2)]

testB :: SignedMultiset Char
testB = fromListWith (+) [('a', -1), ('b', 1)]

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

cardinality :: SignedMultiset a -> Integer
cardinality (SignedMultiset s) =
    Data.Monoid.getSum $ Foldable.foldl' (+) 0 s

magnitude :: SignedMultiset a -> Natural
magnitude (SignedMultiset s) =
    Data.Monoid.getSum $ foldMap (coerce integerMagnitude) s

fromUnsignedPairWith
    :: forall a
     . Ord a
    => (Integer -> Integer -> Integer)
    -> (Multiset a, Multiset a)
    -> SignedMultiset a
fromUnsignedPairWith f (s1, s2) =
    fromListWith f (ns <> ps)
  where
    ns :: [(a, Integer)]
    ns = fmap naturalToNegativeInteger <$> Multiset.toList s1
    ps :: [(a, Integer)]
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
    SignedMultiset $ MonoidMap.intersectionWith min m1 m2

intersections
    :: Foldable1 f => Ord a => f (SignedMultiset a) -> SignedMultiset a
intersections = Foldable1.foldl1' intersection

isSubsetOf :: Ord a => SignedMultiset a -> SignedMultiset a -> Bool
isSubsetOf s1 s2 =
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
