{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Internal.Data.Sign.New
    ( Sign (..)
    , Zero (..)
    , SignZ (..)
    , HasSign (..)
    , HasMagnitude (..)
    , fromMaybe
    , toMaybe
    )
where

import Control.DeepSeq
    ( NFData
    )
import Data.Coerce
    ( coerce
    )
import Data.Functor.Identity
    ( Identity (Identity)
    )
import Data.Group
    ( Abelian
    , Cyclic (generator)
    , Group
    )
import Data.Group qualified as Group
    ( Group (..)
    )
import Data.Int
    ( Int16
    , Int32
    , Int64
    , Int8
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.Monoid.Null qualified as MonoidNull
import Data.Semigroup
    ( Max (Max)
    , Min (Min)
    )
import Data.Semiring
    ( Ring (..)
    , Semiring (..)
    )
import Data.Word
    ( Word16
    , Word32
    , Word64
    , Word8
    )
import GHC.Generics
    ( Generic
    )
import Internal.Data.Monoid
    ( Product (..)
    , Sum (..)
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( null
    )

data Sign
    = N
    | P
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
    deriving anyclass (NFData)

data Zero = Z
    deriving stock (Eq, Generic, Read, Show)

data SignZ
    = Zero !Zero
    | Sign !Sign
    deriving stock (Eq, Generic, Read, Show)

instance Ord SignZ where
    Sign N <= ______ = True
    Zero Z <= Sign N = False
    Zero Z <= ______ = True
    Sign P <= Sign P = True
    Sign P <= ______ = False

instance Bounded SignZ where
    minBound = Sign N
    maxBound = Sign P

instance Enum SignZ where
    fromEnum = fromEnumZ
    toEnum = toEnumZ
    succ = succZ
    pred = predZ

instance Semiring SignZ where
    zero = Zero Z
    one = Sign P
    plus = addZ
    times = multiplyZ

instance Ring SignZ where
    negate = invertZ

--------------------------------------------------------------------------------
-- Sum
--------------------------------------------------------------------------------

instance Semigroup (Sum SignZ) where
    (<>) = coerce addZ

instance Monoid (Sum SignZ) where
    mempty = coerce (Zero Z)

instance MonoidNull (Sum SignZ) where
    null = coerce nullZ

instance Group (Sum SignZ) where
    invert = coerce invertZ

instance Abelian (Sum SignZ)

instance Cyclic (Sum SignZ) where
    generator = coerce (Sign P)

--------------------------------------------------------------------------------
-- Product
--------------------------------------------------------------------------------

instance Semigroup (Product SignZ) where
    (<>) = coerce multiplyZ

instance Monoid (Product SignZ) where
    mempty = coerce (Sign P)

instance MonoidNull (Product SignZ) where
    null (Product s) = s == Sign P

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

fromEnumZ :: SignZ -> Int
fromEnumZ = \case
    Sign N -> -1
    Zero Z -> 00
    Sign P -> 01

toEnumZ :: Integral a => a -> SignZ
toEnumZ i =
    case (i `mod` 3) of
        0 -> Zero Z
        1 -> Sign P
        _ -> Sign N

fromMaybe :: Maybe Sign -> SignZ
fromMaybe = \case
    Nothing -> Zero Z
    Just s_ -> Sign s_

toMaybe :: SignZ -> Maybe Sign
toMaybe = \case
    Sign N -> Just N
    Zero Z -> Nothing
    Sign P -> Just P

succZ :: SignZ -> SignZ
succZ = \case
    Sign N -> Zero Z
    Zero Z -> Sign P
    Sign P -> Sign N

predZ :: SignZ -> SignZ
predZ = \case
    Sign N -> Sign P
    Zero Z -> Sign N
    Sign P -> Zero Z

nullZ :: SignZ -> Bool
nullZ (Zero _) = True
nullZ (Sign _) = False

invert :: Sign -> Sign
invert N = P
invert P = N

invertZ :: SignZ -> SignZ
invertZ (Sign s) = Sign (invert s)
invertZ (Zero Z) = Zero Z

addZ :: SignZ -> SignZ -> SignZ
addZ (Zero Z) anything = anything
addZ anything (Zero Z) = anything
addZ (Sign N) (Sign P) = Zero Z
addZ (Sign P) (Sign N) = Zero Z
addZ (Sign N) (Sign N) = Sign P
addZ (Sign P) (Sign P) = Sign N

multiplyZ :: SignZ -> SignZ -> SignZ
multiplyZ (Zero Z) anything = Zero Z
multiplyZ anything (Zero Z) = Zero Z
multiplyZ (Sign P) anything = anything
multiplyZ anything (Sign P) = anything
multiplyZ (Sign N) (Sign N) = Sign P

--------------------------------------------------------------------------------
-- HasSign
--------------------------------------------------------------------------------

class HasSign a where
    type SignType a
    signOf :: a -> SignType a

instance HasSign Sign where
    type SignType Sign = Sign
    signOf = id

instance HasSign SignZ where
    type SignType SignZ = SignZ
    signOf = id

newtype HasSignNumEq a = HasSignNumEq a

instance (Num a, Ord a) => HasSign (HasSignNumEq a) where
    type SignType (HasSignNumEq a) = SignZ
    signOf (HasSignNumEq a) =
        case signum a of
            (-1) -> Sign N
            (00) -> Zero Z
            (01) -> Sign P
            (__) -> error "HasSignNumEq: signnum post-condition violated"

newtype HasSignNumOrd a = HasSignNumOrd a

instance (Num a, Ord a) => HasSign (HasSignNumOrd a) where
    type SignType (HasSignNumOrd a) = SignZ
    signOf (HasSignNumOrd a) =
        case compare (signum a) 0 of
            LT -> Sign N
            EQ -> Zero Z
            GT -> Sign P

{- ORMOLU_DISABLE -}
deriving via HasSignNumEq  Integer instance HasSign Integer
deriving via HasSignNumEq  Int     instance HasSign Int
deriving via HasSignNumEq  Int8    instance HasSign Int8
deriving via HasSignNumEq  Int16   instance HasSign Int16
deriving via HasSignNumEq  Int32   instance HasSign Int32
deriving via HasSignNumEq  Int64   instance HasSign Int64
deriving via HasSignNumOrd Float   instance HasSign Float
deriving via HasSignNumOrd Double  instance HasSign Double
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
deriving newtype instance HasSign a => HasSign (Identity a)
deriving newtype instance HasSign a => HasSign (Max a)
deriving newtype instance HasSign a => HasSign (Min a)
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Magnitude
--------------------------------------------------------------------------------

class HasMagnitude i where
    type Magnitude i
    magnitude :: i -> Magnitude i

instance HasMagnitude SignZ where
    type Magnitude SignZ = Bool
    magnitude = \case
        Sign N -> True
        Zero Z -> False
        Sign P -> True

instance HasMagnitude Integer where
    type Magnitude Integer = Natural
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int where
    type Magnitude Int = Word
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int8 where
    type Magnitude Int8 = Word8
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int16 where
    type Magnitude Int16 = Word16
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int32 where
    type Magnitude Int32 = Word32
    magnitude = magnitudeIntegralNum

instance HasMagnitude Int64 where
    type Magnitude Int64 = Word64
    magnitude = magnitudeIntegralNum

magnitudeIntegralNum :: (Integral a, Num b) => a -> b
magnitudeIntegralNum i = fromIntegral $ abs i
