{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,GeneralizedNewtypeDeriving #-}

module Control.Monad.VectorSpace where

import Control.Monad
import qualified Data.Map as M

infixl 5 .+
infixl 6 .*

-- | Swaps the elements in a pair. 
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- | A type class for vector spaces v over a base field k.
class Num k => VectorSpace k v | v -> k where
    vzero :: v
    (.+) :: v -> v -> v
    (.*) :: k -> v -> v
    (.-) :: v -> v -> v
    v1 .- v2 = v1 .+ ((-1).*v2)

-- | A concrete instance of VectorSpace class represented as a list of pairs of coefficients and basis elements.
data V k a = V { unV :: [(k,a)] } deriving (Show)

-- | Sorts axis based on basis elements, adds up the coefficients with the same basis and removes axis with zero coefficients. 
reduce x = filter ((/=0) . fst) $ fmap swap $ M.toList $ M.fromListWith (+) $ fmap swap $ x

-- | Vector space are functors.
instance Num k => Functor (V k) where
    fmap f (V as) = V $ map (\(k,a) -> (k,f a)) as

-- |Vector space is a monad.
instance Num k => Monad (V k) where
    return a = V [(1,a)]
    x >>= f = join (fmap f x)
        where join x = V $ concat $ fmap (uncurry scale) $ unV $ fmap unV x
              scale k1 as = map (\(k2,a) -> (k1*k2,a)) as

-- | Vector space V is even a monadplus.
instance Num r => MonadPlus (V r) where
    mzero = V []
    mplus (V x) (V y) = V (x++y)

-- |Instantiation of the vector space type.
instance (Num k,Ord a) => VectorSpace k (V k a) where
    vzero = V []
    V x .+ V y = V (x ++ y)
    (.*) k = (>>= (\a -> V [(k,a)]))

-- | Finds a coefficient of the given basis vector within the VectorSpace V or returns zero in case it does not exist.
coefficient b (V bs) = maybe 0 id (lookup b (map swap (reduce bs)))
