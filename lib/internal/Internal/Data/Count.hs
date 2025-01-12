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
import Data.Monoid.Monus
    ( Monus
    , OverlappingGCDMonoid
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
    ( Semiring
    )
import Internal.Data.Monoid
    ( Sum (Sum)
    )
import Internal.Data.Packed
    ( Packed (Unpacked)
    )
import Internal.Data.Sign
    ( Sign
    )
import Internal.Data.Sign qualified as Sign
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Count a = Count a
    deriving stock (Eq, Ord, Functor)
    deriving newtype (Bounded, Enum, NFData, Semiring, Show)

instance Packed (Count a) where
    type Unpacked (Count a) = a

-- TODO:
-- Think of a way to avoid creating instances for `Count Bool`.
{- ORMOLU_DISABLE -}
deriving via Any instance Semigroup  (Count Bool)
deriving via Any instance Monoid     (Count Bool)
deriving via Any instance MonoidNull (Count Bool)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via Sum Sign instance Semigroup   (Count Sign)
deriving via Sum Sign instance Commutative (Count Sign)
deriving via Sum Sign instance Monoid      (Count Sign)
deriving via Sum Sign instance MonoidNull  (Count Sign)
deriving via Sum Sign instance Group       (Count Sign)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via Monoid.Sum Integer instance Semigroup            (Count Integer)
deriving via Monoid.Sum Integer instance Commutative          (Count Integer)
deriving via Monoid.Sum Integer instance Monoid               (Count Integer)
deriving via Monoid.Sum Integer instance MonoidNull           (Count Integer)
deriving via Monoid.Sum Integer instance Group                (Count Integer)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving via Monoid.Sum Natural instance Semigroup            (Count Natural)
deriving via Monoid.Sum Natural instance Commutative          (Count Natural)
deriving via Monoid.Sum Natural instance Monoid               (Count Natural)
deriving via Monoid.Sum Natural instance MonoidNull           (Count Natural)
deriving via Monoid.Sum Natural instance Monus                (Count Natural)
deriving via Monoid.Sum Natural instance OverlappingGCDMonoid (Count Natural)
deriving via Monoid.Sum Natural instance Reductive            (Count Natural)
deriving via Monoid.Sum Natural instance LeftReductive        (Count Natural)
deriving via Monoid.Sum Natural instance RightReductive       (Count Natural)
deriving via Monoid.Sum Natural instance Cancellative         (Count Natural)
deriving via Monoid.Sum Natural instance LeftCancellative     (Count Natural)
deriving via Monoid.Sum Natural instance RightCancellative    (Count Natural)
deriving via Monoid.Sum Natural instance PositiveMonoid       (Count Natural)
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
instance CountMagnitude Sign where
    countToInteger (Count s) = case s of
        Sign.N -> -1
        Sign.Z ->  0
        Sign.P ->  1
    countToNatural (Count s) = case s of
        Sign.N ->  1
        Sign.Z ->  0
        Sign.P ->  1
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
