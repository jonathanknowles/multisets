{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SignSpec where

import Data.Semigroup
    ( Max (..)
    , Min (..)
    )
import Data.Set.Transformers
    ( Product
    , Sum
    )
import Generators ()
import Internal.Data.Sign
    ( SignOrZero
    )
import Test.Hspec
    ( Spec
    , describe
    )
import Test.QuickCheck.Classes
    ( eqLaws
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
import Prelude

spec :: Spec
spec = describe "Class laws" $ do
    testLawsMany @SignOrZero
        [ eqLaws
        , ordLaws
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
