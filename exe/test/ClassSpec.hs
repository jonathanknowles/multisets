{-# OPTIONS_GHC -Wno-orphans #-}

module ClassSpec where

import Data.Bag
    ( Bag
    )
import Data.Bag qualified as Bag
import Data.Data
    ( Typeable
    , typeRep
    )
import Data.Proxy
    ( Proxy (Proxy)
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( Spec
    , describe
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary, shrink)
    , arbitrarySizedNatural
    , listOf
    , scale
    , shrinkIntegral
    , shrinkMapBy
    )
import Test.QuickCheck.Classes
    ( eqLaws
    , isListLaws
    , monoidLaws
    , semigroupLaws
    , semigroupMonoidLaws
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany
    )
import Test.QuickCheck.Classes.Monoid.GCD
    ( distributiveGCDMonoidLaws
    , gcdMonoidLaws
    , leftDistributiveGCDMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightDistributiveGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.LCM
    ( distributiveLCMMonoidLaws
    , lcmMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.Monus
    ( monusLaws
    )
import Test.QuickCheck.Classes.Monoid.Null
    ( monoidNullLaws
    , positiveMonoidLaws
    )
import Test.QuickCheck.Classes.Semigroup.Cancellative
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
import Prelude

spec :: Spec
spec = do
    describe "Class laws" $ do
        -- Test against a variety of element types, in ascending order of
        -- cardinality:
        specLawsFor (Proxy @())
        specLawsFor (Proxy @Bool)
        specLawsFor (Proxy @Ordering)
        specLawsFor (Proxy @Int)
        specLawsFor (Proxy @Integer)

specLawsFor
    :: forall a
     . ( Arbitrary a
       , Ord a
       , Read a
       , Show a
       , Typeable a
       )
    => Proxy a
    -> Spec
specLawsFor elementType = do
    let description =
            "Class laws for element type " <> show (typeRep elementType)

    describe description $ do

        testLawsMany @(Bag a)
            [ cancellativeLaws
            , commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , eqLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , isListLaws
            , leftCancellativeLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

instance (Arbitrary a, Ord a) => Arbitrary (Bag a) where
    arbitrary =
        Bag.fromList
            <$> scale (`mod` 16) (listOf ((,) <$> arbitrary <*> arbitrary))
    shrink =
        shrinkMapBy Bag.fromMap Bag.toMap shrink
