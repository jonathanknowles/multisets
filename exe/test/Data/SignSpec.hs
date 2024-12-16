{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SignSpec where

import Data.Semigroup
    ( Max (Max)
    , Min (Min)
    )
import Data.Sign
    ( Product (Product)
    , Sign
    , StrictSign
    , Sum (Sum)
    )
import Test.Hspec
    ( Spec
    , describe
    )
import Test.QuickCheck
    ( Arbitrary
        ( arbitrary
        , shrink
        )
    , Property
    , arbitraryBoundedEnum
    , shrinkBoundedEnum
    )
import Test.QuickCheck.Classes
    ( Laws (Laws)
    , monoidLaws
    , semigroupLaws
    )
import Test.QuickCheck.Classes.Group
    ( groupLaws
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany
    )
import Test.QuickCheck.Property
    ( Result (..)
    , mapTotalResult
    )
import Prelude

spec :: Spec
spec = do
    describe "Class laws" $ do
        testLawsMany @(Max Sign)
            [ semigroupLaws
            , monoidLaws
            ]
        testLawsMany @(Min Sign)
            [ semigroupLaws
            , monoidLaws
            ]
        testLawsMany @(Sum Sign)
            [ semigroupLaws
            , monoidLaws
            , groupLaws
            ]
        testLawsMany @(Min StrictSign)
            [ semigroupLaws
            , monoidLaws
            ]
        testLawsMany @(Max StrictSign)
            [ semigroupLaws
            , monoidLaws
            ]
        testLawsMany @(Product StrictSign)
            [ semigroupLaws
            , monoidLaws
            ]
        testLawsMany @(Product StrictSign) $
            disableCoverageCheck
                [ groupLaws
                ]

instance Arbitrary Sign where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

instance Arbitrary StrictSign where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

deriving newtype instance Arbitrary (Min Sign)

deriving newtype instance Arbitrary (Max Sign)

deriving newtype instance Arbitrary (Sum Sign)

deriving newtype instance Arbitrary (Min StrictSign)

deriving newtype instance Arbitrary (Max StrictSign)

deriving newtype instance Arbitrary (Product StrictSign)

--------------------------------------------------------------------------------
-- Coverage checks
--------------------------------------------------------------------------------

class HasCoverageCheck p where
    disableCoverageCheck :: p -> p

instance HasCoverageCheck Laws where
    disableCoverageCheck (Laws title laws) =
        Laws title $ fmap disableCoverageCheck <$> laws

instance HasCoverageCheck Property where
    disableCoverageCheck =
        mapTotalResult (\r -> r {maybeCheckCoverage = Nothing})

instance (Functor f, HasCoverageCheck p) => HasCoverageCheck (f p) where
    disableCoverageCheck =
        fmap disableCoverageCheck
