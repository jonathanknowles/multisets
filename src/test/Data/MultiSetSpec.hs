{-# OPTIONS_GHC -Wno-orphans#-}

-- |
-- Copyright: © 2024 Jonathan Knowles
-- License: Apache-2.0
--
module Data.MultiSetSpec
    ( spec
    ) where

import Prelude

import Data.Function
    ( (&) )
import Data.MultiSet
    ( MultiSetZ, fromListZ, toMultiSetN, toMultiSetZ )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (arbitrary), Property, checkCoverage, cover, property, (===) )

import qualified Data.MultiSet as MultiSet

spec :: Spec
spec = do
    describe "Conversions" $ do
        it "prop_toMultiSetZ_toMultiSetN" $
            prop_toMultiSetZ_toMultiSetN
                & property

prop_toMultiSetZ_toMultiSetN :: MultiSetZ Char -> Property
prop_toMultiSetZ_toMultiSetN m =
    toMultiSetZ (toMultiSetN m) === m
        & cover 10
            ((MultiSet.minimum m < 0) && (MultiSet.maximum m > 0))
            "(MultiSet.minimum m < 0) && (MultiSet.maximum m > 0)"
        & checkCoverage

instance (Arbitrary a, Ord a) => Arbitrary (MultiSetZ a) where
    arbitrary = fromListZ <$> arbitrary
