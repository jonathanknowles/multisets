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

{- ORMOLU_DISABLE -}
unpacked
    :: (  Packed p,     Packed q)
    => (Unpacked p -> Unpacked q)
    -> (         p ->          q)
unpacked f = pack . f . unpack
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
unpacked2
    :: (  Packed p,     Packed q,     Packed r)
    => (Unpacked p -> Unpacked q -> Unpacked r)
    -> (         p ->          q ->          r)
unpacked2 f p1 p2 = pack (f (unpack p1) (unpack p2))
{- ORMOLU_ENABLE -}
