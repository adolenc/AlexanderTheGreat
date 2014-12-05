{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,GeneralizedNewtypeDeriving #-}

module Control.Monad.VectorSpace where

import Control.Monad
import qualified Data.Map as M

infixl 5 .+
infixl 6 .*

swap (x,y) = (y,x)

class Num k => VectorSpace k v | v -> k where
    vzero :: v
    (.+) :: v -> v -> v
    (.*) :: k -> v -> v
    (.-) :: v -> v -> v
    v1 .- v2 = v1 .+ ((-1).*v2)

data V k a = V { unV :: [(k,a)] } deriving (Show)

-- uredi bazne vektorje [(3,z),(1,x),(0,y),(2,x)] -> [(3,x), (3,z)] (sesteje istolezne, izbrise nicelne, sortira
-- fmap swap $ x : zamenja tuple poziciji (1,x) -> (x,1)
-- fromListWith (+) sesteje glede na kljuc na 1. poziciji (x,1), (x,3) -> (x,4), vrne map drevo
-- toList pretvori nazaj v list, fmap swap zopet zamenja poziciji
-- na koncu se odstrani tiste z 0 vrednostjo na 1. poziciji
reduce x = filter ((/=0) . fst) $ fmap swap $ M.toList $ M.fromListWith (+) $ fmap swap $ x

instance Num k => Functor (V k) where
    fmap f (V as) = V $ map (\(k,a) -> (k,f a)) as

instance Num k => Monad (V k) where
    return a = V [(1,a)]
    x >>= f = join (fmap f x)
        where join x = V $ concat $ fmap (uncurry scale) $ unV $ fmap unV x
              scale k1 as = map (\(k2,a) -> (k1*k2,a)) as

instance Num r => MonadPlus (V r) where
    mzero = V []
    mplus (V x) (V y) = V (x++y)

instance (Num k,Ord a) => VectorSpace k (V k a) where
    vzero = V []
    V x .+ V y = V (x ++ y)
    (.*) k = (>>= (\a -> V [(k,a)]))

-- map swap zopet zamenja poziciji (1,x) -> (x,1)
-- lookup poisce par (b,v), vrne value v
-- id - identity funkcija, id x vrne x
-- maybe 0 v vrne 0, če ne najde kljuca b, sicer vrne v
coefficient b (V bs) = maybe 0 id (lookup b (map swap (reduce bs)))
