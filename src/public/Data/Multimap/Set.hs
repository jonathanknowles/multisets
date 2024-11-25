module Data.Multimap.Set where

import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Set
    ( Set
    )
import Data.Set qualified as Set
import Prelude

newtype SetMultimap k v = SetMultimap (MonoidMap k (Set v))
    deriving stock Eq

instance (Show k, Show v) => Show (SetMultimap k v) where
    show m = "fromList " <> show (toList m)

fromList :: (Ord k, Ord v) => [(k, [v])] -> SetMultimap k v
fromList kvs = SetMultimap $ MonoidMap.fromList $ fmap Set.fromList <$> kvs

toList :: SetMultimap k v -> [(k, [v])]
toList (SetMultimap m) = fmap Set.toList <$> MonoidMap.toList m

singleton :: (Ord k, Ord v) => k -> v -> SetMultimap k v
singleton k = SetMultimap . MonoidMap.singleton k . Set.singleton
