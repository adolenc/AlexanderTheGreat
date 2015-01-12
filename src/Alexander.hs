-- | Main module of the library for converting a sensible representation of
-- rational tangles into a series of moves required to untangle it.
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

-- | Datatype representing moves allowed in tangling/untangling
data KnotMove = Twist | Antitwist | Rotate | Antirotate deriving (Eq, Show)

-- | Converts a 'Tangle' into his representation with 'Rational' number.
value :: Tangle -> Rational
value p =
  let (a,b) = value' p
  in re2 $ realPart $ -i*a/b

-- | Auxilary function
value' p =
  let alpha = coefficient (False,False) $ p (False,False)
      beta  = coefficient (True,False)  $ p (False,True)
  in (alpha,beta)

-- | Converts a tangle represented by a 'Rational' number into a series of
-- 'KnotMove's.
steps :: Rational -> [KnotMove]
steps 0 = []
steps q | q<= -1 = Twist:steps (q+1)
        | -1<q && q<1 = Rotate:steps (-1/q)
        | q>=1 = Antitwist:steps (q-1)

-- | 'untangle' is the important function. It takes a tangle, which can be
-- constructed via the monad, and returns a series of 'KnotMove's required to
-- untangle it.
untangle :: Tangle -> [KnotMove]
untangle t = steps (value t)
