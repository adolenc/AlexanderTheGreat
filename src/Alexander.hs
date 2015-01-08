module Alexander
(
  module Data.Tangle
, KnotMove(..)
, untangle
)
where

import Data.Tangle
import Data.KnotComplex
import Control.Monad.VectorSpace

data KnotMove = Twist | Antitwist | Rotate | Antirotate deriving (Eq, Show)

value :: Tangle -> Rational
value p =
  let (a,b) = value' p
  in re2 $ realPart $ -i*a/b
  
value' p =
  let alpha = coefficient (False,False) $ p (False,False)
      beta  = coefficient (True,False)  $ p (False,True)
  in (alpha,beta)

steps :: Rational -> [KnotMove]
steps 0 = []
steps q | q<= -1 = Twist:steps (q+1)
        | -1<q && q<1 = Rotate:steps (-1/q)
        | q>=1 = Antitwist:steps (q-1)

untangle t = steps (value t)

