module Internal.Shared where

import Internal.Data.CountMap
    ( CountMap
    )
import Internal.Data.CountMap qualified as CountMap
import Internal.Data.Packed
    ( Packed (Unpacked)
    )
import Internal.Data.Sign
    ( Sign
    )
import Numeric.Natural
    ( Natural
    )
import Prelude

newtype Bag a = Bag (CountMap a Natural)
    deriving newtype (Eq)

newtype SignedBag a = SignedBag (CountMap a Integer)
    deriving newtype (Eq)

newtype SignedSet a = SignedSet (CountMap a Sign)
    deriving newtype (Eq)

instance Packed (Bag a) where
    type Unpacked (Bag a) = CountMap a Natural

instance Packed (SignedBag a) where
    type Unpacked (SignedBag a) = CountMap a Integer

instance Packed (SignedSet a) where
    type Unpacked (SignedSet a) = CountMap a Sign

instance Show a => Show (Bag a) where
    show = CountMap.showWith "Bag" "(+)"

instance Show a => Show (SignedBag a) where
    show = CountMap.showWith "SignedBag" "(+)"

instance Show a => Show (SignedSet a) where
    show = CountMap.showWith "SignedSet" "Sign.add"
