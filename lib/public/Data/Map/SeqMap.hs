module Data.Map.SeqMap where

import Data.Monoid.GCD
    ( LeftDistributiveGCDMonoid
    , LeftGCDMonoid
    , OverlappingGCDMonoid
    , RightDistributiveGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Semigroup.Cancellative
    ( LeftCancellative
    , LeftReductive
    , RightCancellative
    , RightReductive
    )
import Data.Semigroup.Cancellative qualified as C
import Data.Sequence
    ( Seq
    )
import Internal.Data.Packed
    ( Packed (Unpacked)
    )
import Prelude

newtype SeqMap k v = SeqMap (MonoidMap k (Seq v))
    deriving newtype
        ( Eq
        , LeftCancellative
        , LeftDistributiveGCDMonoid
        , LeftGCDMonoid
        , LeftReductive
        , Monoid
        , MonoidNull
        , OverlappingGCDMonoid
        , PositiveMonoid
        , RightCancellative
        , RightDistributiveGCDMonoid
        , RightGCDMonoid
        , RightReductive
        , Semigroup
        , Show
        )

instance Packed (SeqMap k v) where
    type Unpacked (SeqMap k v) = MonoidMap k (Seq v)

instance Foldable (SeqMap k) where
    foldMap f (SeqMap m) = foldMap (foldMap f) m

instance Functor (SeqMap k) where
    fmap f (SeqMap m) = SeqMap $ MonoidMap.map (fmap f) m

instance Traversable (SeqMap k) where
    traverse f (SeqMap m) = SeqMap <$> MonoidMap.traverse (traverse f) m

fromList :: Ord k => [(k, Seq v)] -> SeqMap k v
fromList = SeqMap . MonoidMap.fromList

toList :: SeqMap k v -> [(k, Seq v)]
toList (SeqMap m) = MonoidMap.toList m

isPrefixOf :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Bool
isPrefixOf = C.isPrefixOf

isProperPrefixOf :: SeqMap k v -> SeqMap k v -> Bool
isProperPrefixOf = undefined

isSuffixOf :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Bool
isSuffixOf = C.isSuffixOf

isProperSuffixOf :: SeqMap k v -> SeqMap k v -> Bool
isProperSuffixOf = undefined
