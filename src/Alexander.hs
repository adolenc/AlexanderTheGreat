module Alexander
(
  module Data.Tangle
, untangle
)
where

import Data.Tangle
import Data.KnotComplex
import Control.Monad.VectorSpace

value :: Tangle -> Rational
value p =
  let (a,b) = value' p
  in re2 $ realPart $ -i*a/b
  
value' p =
  let alpha = coefficient (False,False) $ p (False,False)
      beta  = coefficient (True,False)  $ p (False,True)
  in (alpha,beta)

steps :: Rational -> [String]
steps 0 = []
steps q | q<= -1 = "twist":steps (q+1)
        | -1<q && q<1 = "rotate":steps (-1/q)
        | q>=1 = "antitwist":steps (q-1)

untangle t = steps (value t)

