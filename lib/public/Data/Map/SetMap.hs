module Data.Map.SetMap where

import Data.Bag.Internal
    ( Bag (Bag)
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Set
    ( Set
    )
import Data.Set qualified as Set
import Prelude
import qualified Data.Foldable as F

newtype SetMap k v = SetMap (MonoidMap k (Set v))

keysSet :: SetMap k v -> Set k
keysSet (SetMap m) = MonoidMap.nonNullKeys m

keysBag :: SetMap k v -> Bag k
keysBag (SetMap m) = Bag (MonoidMap.map (fromIntegral . Set.size) m)

valuesSet :: Ord v => SetMap k v -> Set v
valuesSet (SetMap m) = F.fold m

valuesBag :: SetMap k v -> Bag v
valuesBag = undefined
