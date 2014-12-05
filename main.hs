module Main where

import Src.Control.Monad.VectorSpace
import Prelude
import Data.Ratio hiding (numerator,denominator)


type KnotMonad = V L
type L = Complex K

data K = K { re2::Rational, im2::Rational } deriving (Eq,Show)

instance Num K where
  K a b + K a' b' = K (a+a') (b+b')
  K a b * K a' b' = K (a*a'+2*b*b') (a*b'+a'*b)
  negate (K a b) = K (negate a) (negate b)
  abs _ = error ""
  signum _ = error ""
  fromInteger i = K (fromInteger i) 0

instance Fractional K where
  recip (K a b) = let r = recip (a*a-2*b) in K (r*a) (-r*b)
  fromRational x = K x 0

data Complex a = (:+) { realPart :: a, imagPart :: a } deriving (Eq,Show)

hr2 = K 0 (1%2)
a = hr2 :+ hr2
b = hr2 :+ (-hr2)
i = 0 :+ 1

type Tangle = (Bool,Bool) -> KnotMonad (Bool,Bool)

instance Num a => Num (Complex a) where
  (a :+ b) + (a' :+ b') = (a+a') :+ (b+b')
  (a :+ b) * (a' :+ b') = (a*a'-b*b') :+ (a*b'+a'*b)
  negate (a :+ b) = (-a) :+ (-b)
  fromInteger n = fromInteger n :+ 0
  abs (a :+ b) = undefined
  signum (a :+ b) = undefined

instance Fractional a => Fractional (Complex a) where
  recip (a :+ b) = let r = recip (a*a+b*b) in ((a*r) :+ (-b*r))
  fromRational q = fromRational q :+ 0
  
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
  
value :: Tangle -> Rational
value p =
  let (a,b) = value' p
  in re2 $ realPart $ -i*a/b
  
value' p =
  let alpha = coefficient (False,False) $ p (False,False)
      beta  = coefficient (True,False)  $ p (False,True)
  in (alpha,beta)

infinity (i,j) = return (i,j)
zero (i,j) = do
  cup (i,j)
  cap ()

steps :: Rational -> [String]
steps 0 = []
steps q | q<= -1 = "twist":steps (q+1)
        | -1<q && q<1 = "rotate":steps (-1/q)
        | q>=1 = "antitwist":steps (q-1)

untangle t = steps (value t)

example :: Tangle
example (a,b) = do
  (c,d) <- over (a,b)
  (e,f) <- cap ()
  (g,h) <- over (c,e)
  (i,j) <- over (f,d)
  (m,n) <- cap ()
  (k,l) <- cap ()
  (q,r) <- over (h,k)
  (s,y) <- over (l,i)
  (o,p) <- over (n,g)
  (t,u) <- under (p,q)
  (v,w) <- under (r,s)
  (x,z) <- over (y,j)
  cup (o,t)
  cup (u,v)
  cup (w,x)
  return (m,z)
 
