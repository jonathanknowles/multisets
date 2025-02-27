module Internal.Data.Count where

import Control.DeepSeq
    ( NFData
    )
import Data.Group
    ( Group
    )
import Data.Monoid
    ( Any
    )
import Data.Monoid qualified as Monoid
import Data.Monoid.GCD
    ( GCDMonoid
    , LeftGCDMonoid
    , RightGCDMonoid
    , OverlappingGCDMonoid
    )
import Data.Monoid.LCM
    ( LCMMonoid
    )
import Data.Monoid.Monus
    ( Monus
    )
import Data.Monoid.Null
    ( MonoidNull
    , PositiveMonoid
    )
import Data.Semigroup.Cancellative
    ( Cancellative
    , LeftCancellative
    , LeftReductive
    , Reductive
    , RightCancellative
    , RightReductive
    )
import Data.Semigroup.Commutative
    ( Commutative
    )
import Data.Semiring
    ( Ring
    , Semiring
    )
import Internal.Data.Packed
    ( Packed (Unpacked)
    )
import Internal.Data.Set.Transformers
    ( Sum (Sum)
    )
import Internal.Data.Sign
    ( Sign (Negative, Positive)
    , SignOrZero (Sign, Zero)
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Count a = Count {getCount :: a}
    deriving stock (Eq, Ord, Functor)
    deriving newtype
        (Bounded, NFData, Ring, Semiring, Show)

instance Packed (Count a) where
    type Unpacked (Count a) = a

-- TODO:
-- Think of a way to avoid creating instances for `Count Bool`.
{- ORMOLU_DISABLE -}
deriving via Any instance Monoid     (Count Bool)
deriving via Any instance MonoidNull (Count Bool)
deriving via Any instance Semigroup  (Count Bool)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via Sum SignOrZero instance Commutative (Count SignOrZero)
deriving via Sum SignOrZero instance Group       (Count SignOrZero)
deriving via Sum SignOrZero instance Monoid      (Count SignOrZero)
deriving via Sum SignOrZero instance MonoidNull  (Count SignOrZero)
deriving via Sum SignOrZero instance Semigroup   (Count SignOrZero)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via Monoid.Sum Integer instance Cancellative         (Count Integer)
deriving via Monoid.Sum Integer instance Commutative          (Count Integer)
deriving via Monoid.Sum Integer instance Group                (Count Integer)
deriving via Monoid.Sum Integer instance LeftCancellative     (Count Integer)
deriving via Monoid.Sum Integer instance LeftReductive        (Count Integer)
deriving via Monoid.Sum Integer instance Monoid               (Count Integer)
deriving via Monoid.Sum Integer instance MonoidNull           (Count Integer)
deriving via Monoid.Sum Integer instance Reductive            (Count Integer)
deriving via Monoid.Sum Integer instance RightCancellative    (Count Integer)
deriving via Monoid.Sum Integer instance RightReductive       (Count Integer)
deriving via Monoid.Sum Integer instance Semigroup            (Count Integer)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via Monoid.Sum Natural instance Cancellative         (Count Natural)
deriving via Monoid.Sum Natural instance Commutative          (Count Natural)
deriving via Monoid.Sum Natural instance GCDMonoid            (Count Natural)
deriving via Monoid.Sum Natural instance LCMMonoid            (Count Natural)
deriving via Monoid.Sum Natural instance LeftCancellative     (Count Natural)
deriving via Monoid.Sum Natural instance LeftGCDMonoid        (Count Natural)
deriving via Monoid.Sum Natural instance LeftReductive        (Count Natural)
deriving via Monoid.Sum Natural instance Monoid               (Count Natural)
deriving via Monoid.Sum Natural instance MonoidNull           (Count Natural)
deriving via Monoid.Sum Natural instance Monus                (Count Natural)
deriving via Monoid.Sum Natural instance OverlappingGCDMonoid (Count Natural)
deriving via Monoid.Sum Natural instance PositiveMonoid       (Count Natural)
deriving via Monoid.Sum Natural instance Reductive            (Count Natural)
deriving via Monoid.Sum Natural instance RightCancellative    (Count Natural)
deriving via Monoid.Sum Natural instance RightGCDMonoid       (Count Natural)
deriving via Monoid.Sum Natural instance RightReductive       (Count Natural)
deriving via Monoid.Sum Natural instance Semigroup            (Count Natural)
{- ORMOLU_ENABLE -}

class CountMagnitude c where
    countToInteger :: Count c -> Integer
    countToNatural :: Count c -> Natural

{- ORMOLU_DISABLE -}
instance CountMagnitude Integer where
    countToInteger (Count i) = i
    countToNatural (Count i) = fromIntegral (abs i)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
instance CountMagnitude Natural where
    countToInteger (Count n) = fromIntegral n
    countToNatural (Count n) = n
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
instance CountMagnitude SignOrZero where
    countToInteger (Count s) = case s of
        Sign Negative -> -1
        Zero          ->  0
        Sign Positive ->  1
    countToNatural (Count s) = case s of
        Sign Negative -> 1
        Zero          -> 0
        Sign Positive -> 1
{- ORMOLU_ENABLE -}

class CountSymmetricDifference c where
    type CountSymmetricDifferenceAbsolute c
    countSymmetricDifference
        :: Count c
        -> Count c
        -> Count c
    countSymmetricDifferenceAbsolute
        :: Count c
        -> Count c
        -> Count (CountSymmetricDifferenceAbsolute c)

instance CountSymmetricDifference Integer where
    type CountSymmetricDifferenceAbsolute Integer = Natural
    countSymmetricDifference
        (Count i1)
        (Count i2) = Count $ abs $ i1 - i2
    countSymmetricDifferenceAbsolute
        (Count i1)
        (Count i2) = Count $ fromIntegral $ abs $ i1 - i2

{- ORMOLU_DISABLE -}
instance CountSymmetricDifference Natural where
    type CountSymmetricDifferenceAbsolute Natural = Natural
    countSymmetricDifference (Count n1) (Count n2)
        | n1 > n2   = Count $ n1 - n2
        | otherwise = Count $ n2 - n1
    countSymmetricDifferenceAbsolute = countSymmetricDifference
{- ORMOLU_ENABLE -}
