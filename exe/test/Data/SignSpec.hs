{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SignSpec where

import Internal.Data.Monoid
    ( Max (..)
    , Min (..)
    , Product (..)
    , Sum (..)
    )
import Internal.Data.Sign
    ( SignOrZero
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
    , boundedEnumLaws
    , enumLaws
    , eqLaws
    , monoidLaws
    , ordLaws
    , ringLaws
    , semigroupLaws
    , semiringLaws
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
    testLawsMany @SignOrZero
        [ eqLaws
        , ordLaws
        , enumLaws
        , boundedEnumLaws
        , semiringLaws
        , ringLaws
        ]
    testLawsMany @(Min SignOrZero)
        [ semigroupLaws
        , monoidLaws
        ]
    testLawsMany @(Max SignOrZero)
        [ semigroupLaws
        , monoidLaws
        ]
    testLawsMany @(Sum SignOrZero)
        [ semigroupLaws
        , monoidLaws
        , groupLaws
        ]
    testLawsMany @(Product SignOrZero)
        [ semigroupLaws
        , monoidLaws
        ]

instance Arbitrary SignOrZero where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

deriving newtype instance Arbitrary (Min SignOrZero)

deriving newtype instance Arbitrary (Max SignOrZero)

deriving newtype instance Arbitrary (Sum SignOrZero)

deriving newtype instance Arbitrary (Product SignOrZero)

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
