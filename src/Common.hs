module Common where

-- | Data type for all possible moves we are allowed to do with strings in a tangle
data TangleMove = Twist | Antitwist | Rotate | Antirotate deriving (Eq, Show)
