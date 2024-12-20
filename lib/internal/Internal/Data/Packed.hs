{-# LANGUAGE DefaultSignatures #-}

module Internal.Data.Packed where

import Data.Coerce
    ( Coercible
    , coerce
    )
import Prelude

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

unpacked2 :: Packed w => (Unpacked w -> Unpacked w -> Unpacked w) -> w -> w -> w
unpacked2 f w1 w2 = pack (f (unpack w1) (unpack w2))
