module Data.Tangle where

import Prelude
import Control.Monad.VectorSpace
import Data.KnotComplex
import Data.Ratio hiding (numerator,denominator)

-- | Some useful constants
hr2 = K 0 (1%2)
a = hr2 :+ hr2
b = hr2 :+ (-hr2)
i = 0 :+ 1

-- | KnotMonad is represented as a vector space.
type KnotMonad = V (Complex K)

-- | Function transforming a pair of input 
type Tangle = (Bool,Bool) -> KnotMonad (Bool,Bool)

-- | A component of a tangle with two inputs and no outputs.
cup :: (Bool,Bool) -> KnotMonad ()
cup (u,v) = case (u,v) of
  (False,True) -> (-i * b) .* return ()
  (True,False) -> (i * a) .* return ()
  otherwise -> vzero

-- | A component of a tangle with no inputs and two outputs.
cap :: () -> KnotMonad (Bool,Bool)
cap () = (-i * b) .* return (False,True) .+ (i * a) .* return (True,False)

-- | A component of a tangle representing a twist.
over :: Tangle
over (u,v) = a .* do
  () <- cup (u,v)
  cap ()
  .+
  b .* return (u,v)

-- | A component of a tangle representing an antitwist.
under :: Tangle
under (u,v) = b .* do
  () <- cup (u,v)
  cap ()
  .+
  a .* return (u,v)

