module Data.Set.Signed where

import Data.Foldable1
    ( Foldable1
    )
import Data.Foldable1 qualified as Foldable1
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict
    ( Map
    )
import Data.Map.Strict qualified as Map
import Prelude

newtype SignedSet a = SignedSet (Map a StrictSign)

data StrictSign
    = StrictNegative
    | StrictPositive
    deriving (Bounded, Enum, Eq, Ord)

data Sign
    = Negative
    | Zero
    | Positive

negateStrictSign :: StrictSign -> StrictSign
negateStrictSign = \case
    StrictNegative -> StrictPositive
    StrictPositive -> StrictNegative

maybeStrictSignToSign :: Maybe StrictSign -> Sign
maybeStrictSignToSign = \case
    Just StrictNegative -> Negative
    Just StrictPositive -> Positive
    Nothing -> Zero

signToMaybeStrictSign :: Sign -> Maybe StrictSign
signToMaybeStrictSign = \case
    Negative -> Just StrictNegative
    Positive -> Just StrictPositive
    Zero -> Nothing

fromListWith
    :: Ord a
    => (Sign -> Sign -> Sign)
    -> [(a, Sign)]
    -> SignedSet a
fromListWith f =
    SignedSet
        . Map.mapMaybe signToMaybeStrictSign
        . Map.fromListWith f

lookup :: Ord a => a -> SignedSet a -> Sign
lookup a (SignedSet s) = maybeStrictSignToSign $ Map.lookup a s

negate :: SignedSet a -> SignedSet a
negate (SignedSet s) = SignedSet $ Map.map negateStrictSign s

union :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
union (SignedSet s1) (SignedSet s2) =
    SignedSet $ Map.merge whenMissing whenMissing whenMatched s1 s2
  where
    whenMatched = Map.zipWithMatched $ const max
    whenMissing = Map.filterMissing $ const (== StrictPositive)

intersection :: Ord a => SignedSet a -> SignedSet a -> SignedSet a
intersection (SignedSet s1) (SignedSet s2) =
    SignedSet $ Map.merge whenMissing whenMissing whenMatched s1 s2
  where
    whenMatched = Map.zipWithMatched $ const min
    whenMissing = Map.filterMissing $ const (== StrictNegative)

unions :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
unions = Foldable1.foldl1' union

intersections :: Foldable1 f => Ord a => f (SignedSet a) -> SignedSet a
intersections = Foldable1.foldl1' union
