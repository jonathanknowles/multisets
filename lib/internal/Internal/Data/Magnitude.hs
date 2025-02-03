module Internal.Data.Magnitude where

import Data.Int
    ( Int16
    , Int32
    , Int64
    , Int8
    )
import Data.Word
    ( Word16
    , Word32
    , Word64
    , Word8
    )
import Internal.Data.Sign
    ( NumSign (..)
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

class HasMagnitude i where
    type Magnitude i
    magnitude :: i -> Magnitude i

instance HasMagnitude NumSign where
    type Magnitude NumSign = Bool
    magnitude = \case
        N -> True
        Z -> False
        P -> True

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
