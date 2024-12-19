{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SignSpec where

import Data.Sign
    ( Sign
    )
import Internal.Data.Monoid
    ( Max (..)
    , Min (..)
    , Product (..)
    , Sum (..)
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
    , semigroupLaws, semiringLaws, ringLaws
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
spec = describe "Class laws" $ do
    testLawsMany @Sign
        [ semiringLaws
        , ringLaws
        ]
    testLawsMany @(Min Sign)
        [ semigroupLaws
        , monoidLaws
        ]
    testLawsMany @(Max Sign)
        [ semigroupLaws
        , monoidLaws
        ]
    testLawsMany @(Sum Sign)
        [ semigroupLaws
        , monoidLaws
        , groupLaws
        ]
    testLawsMany @(Product Sign)
        [ semigroupLaws
        , monoidLaws
        ]

instance Arbitrary Sign where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

deriving newtype instance Arbitrary (Min Sign)

deriving newtype instance Arbitrary (Max Sign)

deriving newtype instance Arbitrary (Sum Sign)

deriving newtype instance Arbitrary (Product Sign)

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
