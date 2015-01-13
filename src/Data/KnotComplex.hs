module Data.KnotComplex 
( 
  Complex (..)
, K (..)
) where

import Prelude
import Data.Ratio hiding (numerator,denominator)

-- | Data representing a complex number as a pair of arbitrary types. 
data Complex a = (:+) { realPart :: a, imagPart :: a } deriving (Eq,Show)

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

-- | Data representing a complex number as a pair of rationals.
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
