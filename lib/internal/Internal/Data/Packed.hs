{-# LANGUAGE DefaultSignatures #-}

module Internal.Data.Packed where

import Data.Coerce
    ( Coercible
    , coerce
    )
import Prelude

class Packed p where
    type Unpacked p

    unpack :: p -> Unpacked p
    default unpack :: Coercible p (Unpacked p) => p -> Unpacked p
    unpack = coerce

    pack :: Unpacked p -> p
    default pack :: Coercible (Unpacked p) p => Unpacked p -> p
    pack = coerce

unpacked :: Packed p => (Unpacked p -> Unpacked p) -> p -> p
unpacked f = pack . f . unpack

unpacked2 :: Packed p => (Unpacked p -> Unpacked p -> Unpacked p) -> p -> p -> p
unpacked2 f p1 p2 = pack (f (unpack p1) (unpack p2))
