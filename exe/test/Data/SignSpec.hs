{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SignSpec where

import Internal.Data.Sign.Num
    ( NumSign
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
    testLawsMany @NumSign
        [ semiringLaws
        , ringLaws
        ]
    testLawsMany @(Min NumSign)
        [ semigroupLaws
        , monoidLaws
        ]
    testLawsMany @(Max NumSign)
        [ semigroupLaws
        , monoidLaws
        ]
    testLawsMany @(Sum NumSign)
        [ semigroupLaws
        , monoidLaws
        , groupLaws
        ]
    testLawsMany @(Product NumSign)
        [ semigroupLaws
        , monoidLaws
        ]

instance Arbitrary NumSign where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

deriving newtype instance Arbitrary (Min NumSign)

deriving newtype instance Arbitrary (Max NumSign)

deriving newtype instance Arbitrary (Sum NumSign)

deriving newtype instance Arbitrary (Product NumSign)

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
