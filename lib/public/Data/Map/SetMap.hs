module Data.Map.SetMap where

import Data.Foldable qualified as F
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Set
    ( Set
    )
import Data.Set qualified as Set
import Internal.Data.Count
    ( Count (Count)
    )
import Internal.Shared
    ( Bag (Bag)
    )
import Prelude

newtype SetMap k v = SetMap (MonoidMap k (Set v))

keySet :: SetMap k v -> Set k
keySet (SetMap m) = MonoidMap.nonNullKeys m

keyBag :: SetMap k v -> Bag k
keyBag (SetMap m) = Bag (MonoidMap.map (Count . fromIntegral . Set.size) m)

valueSet :: Ord v => SetMap k v -> Set v
valueSet (SetMap m) = F.fold m

valueBag :: SetMap k v -> Bag v
valueBag = undefined
