module Main where

import Src.Alexander

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
