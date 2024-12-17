module Data.Set.Signed where

import Data.Coerce (coerce)
import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Data.Sign
    ( Sign (..)
    , Sum (..)
    )
import Prelude

newtype SignedSet a = SignedSet (MonoidMap a (Sum Sign))

instance Show a => Show (SignedSet a) where
    show s =
        "SignedSet.fromListWith Sign.add "
            <> show (toList s)

fromListWith
    :: Ord a
    => (Sign -> Sign -> Sign)
    -> [(a, Sign)]
    -> SignedSet a
fromListWith f xs = SignedSet $ MonoidMap.fromListWith (coerce f) (coerce xs)

toList :: forall a. SignedSet a -> [(a, Sign)]
toList =
    coerce
        @(_ -> [(a, Sum Sign)])
        @(_ -> [(a, Sign)])
        MonoidMap.toList

lookup :: Ord a => a -> SignedSet a -> Sign
lookup a (SignedSet s) = coerce $ MonoidMap.get a s

invert :: SignedSet a -> SignedSet a
invert (SignedSet s) = SignedSet $ MonoidMap.invert s

union :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
union (SignedSet s1) (SignedSet s2) =
    SignedSet $ MonoidMap.unionWith max s1 s2

intersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
intersection (SignedSet s1) (SignedSet s2) =
    SignedSet $ MonoidMap.unionWith min s1 s2

unions :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
unions = Foldable1.foldl1' union

intersections :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
intersections = Foldable1.foldl1' union
