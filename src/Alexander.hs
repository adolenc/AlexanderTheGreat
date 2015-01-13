{-|
Module      : Alexander
Description : Library that helps with untangling of the rational tangles.
-}

module Alexander
(
  module Data.Tangle
, TangleMove(..)
, untangle
)
where

import Data.Tangle
import Data.TangleComplex
import Control.Monad.VectorSpace

-- | Data type for all possible moves we are allowed to do with strings in a tangle
data TangleMove = Twist | Antitwist | Rotate | Antirotate deriving (Eq, Show)

-- | Converts a tangle into its representation with a rational number.
value :: Tangle -> Rational
value p =
  let (a,b) = value' p
  in re2 $ realPart $ -i*a/b

-- | Auxilary function.
value' p =
  let alpha = coefficient (False,False) $ p (False,False)
      beta  = coefficient (True,False)  $ p (False,True)
  in (alpha,beta)

-- | Based on a rational number representing a rational tangle it produces the next move to untangle it and calls itself recursively with a number representing the new tangle.
steps :: Rational -> [TangleMove]
steps 0 = []
steps q | q<= -1 = Twist:steps (q+1)
        | -1<q && q<1 = Rotate:steps (-1/q)
        | q>=1 = Antitwist:steps (q-1)

-- | For a given tangle it produces the sequence of moves to untangle it.
untangle t = steps (value t)

