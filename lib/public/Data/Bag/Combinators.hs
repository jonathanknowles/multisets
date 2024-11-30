module Data.Bag.Combinators where

newtype Sum m = Sum
    { getSum :: m
    }

newtype Union m = Union
    { getUnion :: m
    }

newtype Intersection m = Intersection
    { getIntersection :: m
    }
