module Main where

import Prelude
import Src.Data.Tangle
import Src.Data.KnotComplex
import Src.Control.Monad.VectorSpace

value' p =
  let alpha = coefficient (False,False) $ p (False,False)
      beta  = coefficient (True,False)  $ p (False,True)
  in (alpha,beta)

value :: Tangle -> Rational
value p =
  let (a,b) = value' p
  in re2 $ realPart $ -i*a/b
  
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
