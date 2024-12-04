module Data.Bag.Internal where

import Data.Coerce
    ( coerce
    )
import Data.Function
    ( on
    )
import Data.Map.Strict
    ( Map
    )
import Data.Monoid.Null
    ( MonoidNull (null)
    )
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( compare
    , null
    )
import Prelude qualified
import Data.Sign.Internal (Sign (..))

newtype Count a = Count {getCount :: a}
    deriving stock (Bounded, Eq, Ord)
    deriving newtype Num

instance Semigroup (Count Natural) where
    a <> b = a + b

instance Semigroup (Count Integer) where
    a <> b = a + b

{- ORMOLU_DISABLE -}
instance Semigroup (Count Sign) where
    Count Negative <> Count Negative = Count Negative
    Count Negative <> Count Zero     = Count Negative
    Count Negative <> Count Positive = Count Zero
    Count Zero     <> Count Negative = Count Negative
    Count Zero     <> Count Zero     = Count Zero
    Count Zero     <> Count Positive = Count Positive
    Count Positive <> Count Negative = Count Zero
    Count Positive <> Count Zero     = Count Positive
    Count Positive <> Count Positive = Count Positive
{- ORMOLU_ENABLE -}

instance Monoid (Count Natural) where
    mempty = 0

instance Monoid (Count Integer) where
    mempty = 0

instance Monoid (Count Sign) where
    mempty = Count Zero

instance MonoidNull (Count Natural) where
    null (Count 0) = True
    null _ = False

instance MonoidNull (Count Integer) where
    null (Count 0) = True
    null _ = False

instance MonoidNull (Count Sign) where
    null (Count Zero) = True
    null _ = False

newtype Bag a = Bag (MonoidMap a (Count Natural))
    deriving newtype (Eq)

newtype SignedBag a = SignedBag (MonoidMap a (Count Integer))
    deriving newtype (Eq)

newtype SignedSet a = SignedSet (MonoidMap a (Count Sign))
    deriving newtype (Eq)

instance Ord a => Ord (Bag a) where
    compare (Bag b1) (Bag b2) = compare b1 b2

instance Ord a => Ord (SignedBag a) where
    compare (SignedBag b1) (SignedBag b2) = compare b1 b2

instance Ord a => Ord (SignedSet a) where
    compare (SignedSet b1) (SignedSet b2) = compare b1 b2

instance Show a => Show (Bag a) where
    show (Bag s) =
        "Bag.fromListWith (+) "
            <> show (toList s)

instance Show a => Show (SignedBag a) where
    show (SignedBag s) =
        "SignedBag.fromListWith (+) "
            <> show (toList s)

instance Show a => Show (SignedSet a) where
    show (SignedSet s) =
        "SignedSet.fromListWith (+) "
            <> show (toList s)

compare
    :: forall a n
     . (Ord a, Ord n)
    => MonoidMap a (Count n)
    -> MonoidMap a (Count n)
    -> Ordering
compare = Prelude.compare `on` toMap

toList :: forall a n. MonoidMap a (Count n) -> [(a, n)]
toList =
    coerce
        @(_ -> [(a, Count n)])
        @(_ -> [(a, n)])
        MonoidMap.toList

toMap :: forall a n. MonoidMap a (Count n) -> Map a n
toMap =
    coerce
        @(_ -> Map a (Count n))
        @(_ -> Map a n)
        MonoidMap.toMap
