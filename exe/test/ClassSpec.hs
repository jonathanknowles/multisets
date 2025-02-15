{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ClassSpec where

import Data.Bag
    ( Bag
    )
import Data.Bag.Signed
    ( SignedBag
    )
import Data.Data
    ( Typeable
    , typeRep
    )
import Data.Map.SeqMap
    ( SeqMap
    )
import Data.Proxy
    ( Proxy (Proxy)
    )
import Data.Set
    ( Set
    )
import Data.Set.Signed
    ( SignedSet
    )
import Data.Set.Transformers
    ( Intersection (..)
    , Product (..)
    , Sum (..)
    , Union (..)
    )
import Generators ()
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( Spec
    , describe
    )
import Test.QuickCheck
    ( Arbitrary
    )
import Test.QuickCheck.Classes
    ( eqLaws
    , foldableLaws
    , functorLaws
    , isListLaws
    , monoidLaws
    , ordLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showLaws
    , traversableLaws
    )
import Test.QuickCheck.Classes.Group
    ( groupLaws
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
import Test.QuickCheck.Quid
    ( Latin (..)
    , Quid
    , Size (..)
    )
import Prelude

newtype Element (size :: Natural) = Element (Latin Quid)
    deriving stock (Eq, Ord)
    deriving newtype (Read, Show)
    deriving (Arbitrary) via Size size Quid

spec :: Spec
spec = do
    describe "Class laws" $ do
        -- Test against a variety of element sizes:
        specLawsFor (Proxy @(Element 2))
        specLawsFor (Proxy @(Element 4))
        specLawsFor (Proxy @(Element 8))

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
        -- Laws for base types:
        testLawsMany @(Bag a)
            [ eqLaws
            , isListLaws
            , ordLaws
            , showLaws
            ]
        testLawsMany @(SignedBag a)
            [ eqLaws
            , isListLaws
            , ordLaws
            , showLaws
            ]
        testLawsMany @(SignedSet a)
            [ eqLaws
            , isListLaws
            , ordLaws
            , showLaws
            ]

        -- Laws for 'Sum':
        testLawsMany @(Sum (Bag a))
            [ cancellativeLaws
            , commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
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
        testLawsMany @(Sum (SignedBag a))
            [ cancellativeLaws
            , commutativeLaws
            , groupLaws
            , leftCancellativeLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]
        testLawsMany @(Sum (SignedSet a))
            [ commutativeLaws
            , groupLaws
            , monoidLaws
            , monoidNullLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]

        -- Laws for 'Product':
        testLawsMany @(Product (Bag a))
            [ commutativeLaws
            , semigroupLaws
            ]
        testLawsMany @(Product (SignedBag a))
            [ commutativeLaws
            , semigroupLaws
            ]
        testLawsMany @(Product (SignedSet a))
            [ commutativeLaws
            , semigroupLaws
            ]

        -- Laws for 'Union':
        testLawsMany @(Union (Bag a))
            [ commutativeLaws
            , monoidLaws
            , monoidNullLaws
            , positiveMonoidLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]
        testLawsMany @(Union (Set a))
            [ commutativeLaws
            , distributiveGCDMonoidLaws
            , distributiveLCMMonoidLaws
            , gcdMonoidLaws
            , lcmMonoidLaws
            , leftDistributiveGCDMonoidLaws
            , leftGCDMonoidLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightDistributiveGCDMonoidLaws
            , rightGCDMonoidLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            ]
        testLawsMany @(Union (SignedBag a))
            [ commutativeLaws
            , semigroupLaws
            ]
        testLawsMany @(Union (SignedSet a))
            [ commutativeLaws
            , semigroupLaws
            ]

        -- Laws for 'Intersection':
        testLawsMany @(Intersection (Bag a))
            [ commutativeLaws
            , semigroupLaws
            ]
        testLawsMany @(Intersection (SignedBag a))
            [ commutativeLaws
            , semigroupLaws
            ]
        testLawsMany @(Intersection (SignedSet a))
            [ commutativeLaws
            , semigroupLaws
            ]

        testLawsMany @(SeqMap a)
            [ foldableLaws
            , functorLaws
            , traversableLaws
            ]
