module Internal.Data.Semigroup.Transformers
    ( Sum (..)
    , Product (..)
    , Union (..)
    , Intersection (..)
    )
where
import Prelude

newtype Sum a = Sum {getSum :: a}
    deriving stock (Eq, Show)

newtype Product a = Product {getProduct :: a}
    deriving stock (Eq, Show)

newtype Union a = Union {getUnion :: a}
    deriving stock (Eq, Show)

newtype Intersection a = Intersection {getIntersection :: a}
    deriving stock (Eq, Show)
