module Data.Map.SeqMap
    ( SeqMap
    , fromList
    , toList
    , keySet
    , keyBag
    , valueSet
    , valueBag
    , isPrefixOf
    , isSuffixOf
    , isProperPrefixOf
    , isProperSuffixOf
    , stripPrefix
    , stripSuffix
    , commonPrefix
    , commonSuffix
    , stripCommonPrefix
    , stripCommonSuffix
    , overlap
    , stripPrefixOverlap
    , stripSuffixOverlap
    , stripOverlap
    )
where

import Data.Map.Strict qualified as Map
import Data.Monoid.GCD
    ( LeftDistributiveGCDMonoid
    , LeftGCDMonoid
    , OverlappingGCDMonoid
    , RightDistributiveGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.GCD qualified as LeftGCDMonoid
    ( commonPrefix
    , stripCommonPrefix
    )
import Data.Monoid.GCD qualified as OverlappingGCDMonoid
    ( overlap
    , stripOverlap
    , stripPrefixOverlap
    , stripSuffixOverlap
    )
import Data.Monoid.GCD qualified as RightGCDMonoid
    ( commonSuffix
    , stripCommonSuffix
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
import Data.Semigroup.Cancellative qualified as LeftReductive
    ( isPrefixOf
    , stripPrefix
    )
import Data.Semigroup.Cancellative qualified as RightReductive
    ( isSuffixOf
    , stripSuffix
    )
import Data.Sequence
    ( Seq
    )
import Data.Sequence qualified as Seq
import Data.Set
    ( Set
    )
import Internal.Data.Count
    ( Count (Count)
    )
import Internal.Data.Packed
    ( Packed (Unpacked)
    )
import Internal.Shared
    ( Bag (Bag)
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

keySet :: SeqMap k v -> Set k
keySet (SeqMap m) = MonoidMap.nonNullKeys m

keyBag :: Ord k => SeqMap k v -> Bag k
keyBag (SeqMap m) = Bag (MonoidMap.map (Count . fromIntegral . Seq.length) m)

valueSet :: Ord v => SeqMap k v -> Set v
valueSet = undefined

valueBag :: SeqMap k v -> Bag v
valueBag = undefined

isPrefixOf :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Bool
isPrefixOf = LeftReductive.isPrefixOf

isSuffixOf :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Bool
isSuffixOf = RightReductive.isSuffixOf

isProperPrefixOf :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Bool
isProperPrefixOf (SeqMap m1) (SeqMap m2) =
    Map.isProperSubmapOfBy
        LeftReductive.isPrefixOf
        (MonoidMap.toMap m1)
        (MonoidMap.toMap m2)

isProperSuffixOf :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Bool
isProperSuffixOf (SeqMap m1) (SeqMap m2) =
    Map.isProperSubmapOfBy
        RightReductive.isSuffixOf
        (MonoidMap.toMap m1)
        (MonoidMap.toMap m2)

stripPrefix :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Maybe (SeqMap k v)
stripPrefix = LeftReductive.stripPrefix

stripSuffix :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> Maybe (SeqMap k v)
stripSuffix = RightReductive.stripSuffix

commonPrefix :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> SeqMap k v
commonPrefix = LeftGCDMonoid.commonPrefix

commonSuffix :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> SeqMap k v
commonSuffix = RightGCDMonoid.commonSuffix

stripCommonPrefix
    :: (Ord k, Eq v)
    => SeqMap k v
    -> SeqMap k v
    -> (SeqMap k v, SeqMap k v, SeqMap k v)
stripCommonPrefix = LeftGCDMonoid.stripCommonPrefix

stripCommonSuffix
    :: (Ord k, Eq v)
    => SeqMap k v
    -> SeqMap k v
    -> (SeqMap k v, SeqMap k v, SeqMap k v)
stripCommonSuffix = RightGCDMonoid.stripCommonSuffix

overlap :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> SeqMap k v
overlap = OverlappingGCDMonoid.overlap

stripPrefixOverlap :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> SeqMap k v
stripPrefixOverlap = OverlappingGCDMonoid.stripPrefixOverlap

stripSuffixOverlap :: (Ord k, Eq v) => SeqMap k v -> SeqMap k v -> SeqMap k v
stripSuffixOverlap = OverlappingGCDMonoid.stripSuffixOverlap

stripOverlap
    :: (Ord k, Eq v)
    => SeqMap k v
    -> SeqMap k v
    -> (SeqMap k v, SeqMap k v, SeqMap k v)
stripOverlap = OverlappingGCDMonoid.stripOverlap
