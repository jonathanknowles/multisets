module Main where

import Control.DeepSeq
    ( rnf
    )
import Control.Exception
    ( evaluate
    )
import Data.Bag
    ( Bag
    )
import Data.Bag qualified as Bag
import Numeric.Natural
    ( Natural
    )
import Test.Tasty.Bench
    ( bench
    , bgroup
    , defaultMain
    , nf
    )
import Test.Tasty.HUnit
    ( assertEqual
    )
import Prelude

main :: IO ()
main = do
    evaluate $
        rnf
            [ largeBagA
            , smallBagA
            ]

    assertEqual
        "Bag.powersetSize largeBagA"
        (Bag.powersetSize largeBagA)
        largeBagAPowersetSize

    assertEqual
        "Bag.powersetSize smallBagA"
        (Bag.powersetSize smallBagA)
        smallBagAPowersetSize

    defaultMain
        [ bgroup
            "Bag"
            [ bench
                "powerset"
                $ nf (Bag.powerset) smallBagA
            , bench
                "powersetElements"
                $ nf (take 1024 . Bag.powersetElements) largeBagA
            ]
        ]

largeBagA :: Bag Natural
largeBagA = Bag.fromList [(a, 7) | a <- [1 .. 63]]

largeBagAPowersetSize :: Natural
largeBagAPowersetSize =
    784637716923335095479473677900958302012794430558004314112

smallBagA :: Bag Natural
smallBagA = Bag.fromList [(a, 3) | a <- [1 .. 7]]

smallBagAPowersetSize :: Natural
smallBagAPowersetSize = 16384
