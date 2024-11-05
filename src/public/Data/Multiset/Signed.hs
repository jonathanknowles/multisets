module Data.Multiset.Signed where

import Prelude hiding
    ( sum )

import Data.Coerce
    ( coerce )
import Data.MonoidMap
    ( MonoidMap )
import Data.Multiset.Combinators
    ( Sum (..), Union (..), Intersection (..) )

import qualified Data.MonoidMap as MonoidMap
import qualified Data.Monoid
    ( Sum )
import Data.Multiset (Multiset)
import qualified Data.Multiset as Multiset
import Numeric.Natural (Natural)
import Data.List (partition)

newtype SignedMultiset v = SignedMultiset
    (MonoidMap v (Data.Monoid.Sum Integer))

instance Ord v => Semigroup (Sum (SignedMultiset v)) where
    (<>) = coerce sum
instance Ord v => Monoid (Sum (SignedMultiset v)) where
    mempty = coerce empty

instance Ord v => Semigroup (Union (SignedMultiset v)) where
    (<>) = coerce union
instance Ord v => Monoid (Union (SignedMultiset v)) where
    mempty = coerce empty

instance Ord v => Semigroup (Intersection (SignedMultiset v)) where
    (<>) = coerce intersection
instance Ord v => Monoid (Intersection (SignedMultiset v)) where
    mempty = coerce empty

empty :: SignedMultiset v
empty = SignedMultiset MonoidMap.empty

toList :: SignedMultiset v -> [(v, Integer)]
toList = undefined

fromUnsignedPair :: Ord v => (Multiset v, Multiset v) -> SignedMultiset v
fromUnsignedPair = undefined

toUnsignedPair :: Ord v => SignedMultiset v -> (Multiset v, Multiset v)
toUnsignedPair m =
    ( Multiset.fromListSum $
        fmap (fmap (fromIntegral @Integer @Natural . abs))
        ns
    , Multiset.fromListSum $
        fmap (fmap (fromIntegral @Integer @Natural))
        ps
    )
  where
    (ns, ps) = partition ((< 0) . snd) (toList m)

difference
    :: Ord v
    => SignedMultiset v
    -> SignedMultiset v
    -> SignedMultiset v
difference (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ m1 `MonoidMap.minus` m2

sum
    :: Ord v
    => SignedMultiset v
    -> SignedMultiset v
    -> SignedMultiset v
sum (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.unionWith (+) m1 m2

union
    :: Ord v
    => SignedMultiset v
    -> SignedMultiset v
    -> SignedMultiset v
union (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.unionWith max m1 m2

intersection
    :: Ord v
    => SignedMultiset v
    -> SignedMultiset v
    -> SignedMultiset v
intersection (SignedMultiset m1) (SignedMultiset m2) =
    SignedMultiset $ MonoidMap.intersectionWith min m1 m2
