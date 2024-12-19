{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Internal.Shared where

import Data.Coerce (Coercible, coerce)
import Data.Group
    ( Group
    )
import Data.Map.Strict
    ( Map
    )
import Data.Monoid.Null (MonoidNull)
import Data.MonoidMap
    ( MonoidMap
    )
import Data.MonoidMap qualified as MonoidMap
import Internal.Data.Monoid
    ( Sum (..)
    )
import Internal.Data.Sign
    ( Sign
    )
import Numeric.Natural
    ( Natural
    )
import Prelude
import qualified Data.Foldable1 as Foldable1

newtype Count a = Count a
    deriving stock (Eq, Ord, Functor)

{- ORMOLU_DISABLE -}
deriving via Sum Sign instance Semigroup  (Count Sign)
deriving via Sum Sign instance Monoid     (Count Sign)
deriving via Sum Sign instance MonoidNull (Count Sign)
deriving via Sum Sign instance Group      (Count Sign)
{- ORMOLU_ENABLE -}

type CountMap a c = MonoidMap a (Count c)

newtype Bag a = Bag (CountMap a Natural)
    deriving newtype (Eq)

newtype SignedBag a = SignedBag (CountMap a Integer)
    deriving newtype (Eq)

newtype SignedSet a = SignedSet (CountMap a Sign)
    deriving newtype (Eq)

class Packed w where
    type Unpacked w

    unpack :: w -> Unpacked w
    default unpack :: Coercible w (Unpacked w) => w -> Unpacked w
    unpack = coerce

    pack :: Unpacked w -> w
    default pack :: Coercible (Unpacked w) w => Unpacked w -> w
    pack = coerce

unpacked :: Packed w => (Unpacked w -> Unpacked w) -> w -> w
unpacked f = pack . f . unpack

instance Packed (Count a) where
    type Unpacked (Count a) = a

instance Packed (Bag a) where
    type Unpacked (Bag a) = CountMap a Natural

instance Packed (SignedBag a) where
    type Unpacked (SignedBag a) = CountMap a Integer

instance Packed (SignedSet a) where
    type Unpacked (SignedSet a) = CountMap a Sign

type PackedCountMap p k c =
    (Packed p, Unpacked p ~ CountMap k c, MonoidNull (Count c))

instance Show a => Show (Bag a) where
    show (Bag b) = showWith "Bag" "(+)" b

instance Show a => Show (SignedBag a) where
    show (SignedBag b) = showWith "SignedBag" "(+)" b

instance Show a => Show (SignedSet a) where
    show (SignedSet s) = showWith "SignedSet" "Sign.add" s

lookup :: (Ord k, PackedCountMap p k c) => k -> p -> c
lookup a = unpack . MonoidMap.get a . unpack

invert :: (Group (Count c), PackedCountMap p a c) => p -> p
invert = unpacked MonoidMap.invert

union :: (Ord k, Ord c, PackedCountMap p k c) => p -> p -> p
union m1 m2 = pack (MonoidMap.unionWith max (unpack m1) (unpack m2))

intersection :: (Ord k, Ord c, PackedCountMap p k c) => p -> p -> p
intersection m1 m2 = pack (MonoidMap.unionWith min (unpack m1) (unpack m2))

showWith
    :: (Show a, Show c)
    => String
    -> String
    -> CountMap a c
    -> String
showWith typeName operatorName s =
    typeName <> ".fromListWith " <> operatorName <> " " <> show (toList s)

fromListWith
    :: (Ord a, MonoidNull (Count c), Packed w, Unpacked w ~ CountMap a c)
    => (c -> c -> c)
    -> [(a, c)]
    -> w
fromListWith f xs = pack $ MonoidMap.fromListWith (coerce f) (coerce xs)

toList :: forall a c. CountMap a c -> [(a, c)]
toList =
    coerce
        @(_ -> [(a, Count c)])
        @(_ -> [(a, c)])
        MonoidMap.toList

toMap :: forall a c. CountMap a c -> Map a c
toMap =
    coerce
        @(_ -> Map a (Count c))
        @(_ -> Map a c)
        MonoidMap.toMap
