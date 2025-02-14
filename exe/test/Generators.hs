{-# OPTIONS_GHC -Wno-orphans #-}

module Generators where

import Data.Bag
    ( Bag
    )
import Data.Bag qualified as Bag
import Data.Bag.Signed
    ( SignedBag
    )
import Data.Bag.Signed qualified as SignedBag
import Data.Set.Signed
    ( SignedSet
    )
import Data.Set.Signed qualified as SignedSet
import Data.Set.Transformers
    ( Intersection (Intersection)
    , Product (Product)
    , Sum (Sum)
    , Union (Union)
    )
import Data.Sign
    ( Sign (Negative, Positive)
    )
import Internal.Data.Sign
    ( SignOrZero (Sign, Zero)
    )
import Numeric.Natural
    ( Natural
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , arbitrarySizedNatural
    , elements
    , listOf
    , oneof
    , scale
    , shrinkIntegral
    , shrinkMapBy
    )
import Test.QuickCheck.Arbitrary (shrink)
import Prelude
import Data.Semigroup (Min (Min), Max (Max) )

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

instance Arbitrary Sign where
    arbitrary = elements [Negative, Positive]
    shrink = \case
        Negative -> [Positive]
        Positive -> []

instance Arbitrary SignOrZero where
    arbitrary = oneof [pure Zero, Sign <$> arbitrary]
    shrink = \case
        Zero -> []
        Sign s -> Zero : (Sign <$> shrink s)

deriving newtype instance Arbitrary a => Arbitrary (Max a)

deriving newtype instance Arbitrary a => Arbitrary (Min a)

deriving newtype instance Arbitrary a => Arbitrary (Sum a)

deriving newtype instance Arbitrary a => Arbitrary (Product a)

deriving newtype instance Arbitrary a => Arbitrary (Union a)

deriving newtype instance Arbitrary a => Arbitrary (Intersection a)

instance (Arbitrary a, Ord a) => Arbitrary (Bag a) where
    arbitrary =
        Bag.fromList
            <$> scale (`mod` 16) (listOf ((,) <$> arbitrary <*> arbitrary))
    shrink =
        shrinkMapBy Bag.fromMap Bag.toMap shrink

instance (Arbitrary a, Ord a) => Arbitrary (SignedBag a) where
    arbitrary =
        SignedBag.fromList
            <$> scale (`mod` 16) (listOf ((,) <$> arbitrary <*> arbitrary))
    shrink =
        shrinkMapBy SignedBag.fromMap SignedBag.toMap shrink

instance (Arbitrary a, Ord a) => Arbitrary (SignedSet a) where
    arbitrary =
        SignedSet.fromListWith const
            <$> scale (`mod` 16) (listOf ((,) <$> arbitrary <*> arbitrary))
    shrink =
        shrinkMapBy SignedSet.fromMap SignedSet.toMap shrink
