module Main where

import Prelude
import Src.Data.Tangle
import Src.Alexander
import Src.Data.KnotComplex
import Src.Control.Monad.VectorSpace


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
