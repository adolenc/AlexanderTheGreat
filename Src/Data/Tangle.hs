module Src.Data.Tangle where

import Prelude
import Src.Control.Monad.VectorSpace
import Src.Data.KnotComplex
import Data.Ratio hiding (numerator,denominator)

hr2 = K 0 (1%2)
a = hr2 :+ hr2
b = hr2 :+ (-hr2)
i = 0 :+ 1

type L = Complex K
type KnotMonad = V L

type Tangle = (Bool,Bool) -> KnotMonad (Bool,Bool)

cup :: (Bool,Bool) -> KnotMonad ()
cup (u,v) = case (u,v) of
  (False,True) -> (-i * b) .* return ()
  (True,False) -> (i * a) .* return ()
  otherwise -> vzero

cap :: () -> KnotMonad (Bool,Bool)
cap () = (-i * b) .* return (False,True) .+ (i * a) .* return (True,False)

over :: Tangle
over (u,v) = a .* do
  () <- cup (u,v)
  cap ()
  .+
  b .* return (u,v)

under :: Tangle
under (u,v) = b .* do
  () <- cup (u,v)
  cap ()
  .+
  a .* return (u,v)
  
infinity (i,j) = return (i,j)
zero (i,j) = do
  cup (i,j)
  cap ()
