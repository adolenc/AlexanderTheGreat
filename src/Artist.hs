{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,GeneralizedNewtypeDeriving,NoMonomorphismRestriction #-}

-- | Module for drawing rational tangles.

module Artist
(
  TangleMove(..)
, renderTangle
, renderTangleReversed
) where

import Diagrams.Prelude hiding (Point, Line, rotate, shift, distance)
import Diagrams.Backend.SVG

-- | Data type for all possible moves we are allowed to do with strings in a tangle
data TangleMove = Twist | Antitwist | Rotate | Antirotate deriving (Eq)
-- | Data type for current orientation of a tangle, which tells us which string ends are going to be used in next move.
data Orientation = West | North | East | South deriving (Show)
-- | Data type for a string end.
data LinePosition = FirstA | LastA | FirstB | LastB deriving (Show)
-- | Data type for Z axis consisting of only three values.
data Z = Over | Under | Normal deriving (Eq)

-- | Points are declared with x,y coordinate and a special z coordinate. They are used for constructing and shaping a tangle.
type Point = (Double, Double, Z)
-- | Represents one of the strings. Each string is defined as a spline with points used as spline control points.
type Line = [Point]
-- | Tangle is represented as a pair of strings.
type Tangle = (Line, Line)
-- | Represents a tuple of both strings ends. The order tells us the current location of a string end. First position corresponds to NW direction, last one to SW position.
type EdgePoints = (LinePosition, LinePosition, LinePosition, LinePosition)
-- | Represents a knot and current positions of strings ends.
type TangleWithEdges = (Tangle, EdgePoints)

(.+) :: Point -> Point -> Point
(x1, y1, z1) .+ (x2, y2, _) = (x1 + x2, y1 + y2, z1)

-- | Euclidian distance between two points.
distance :: Point -> Point -> Double
distance (x1, y1, _) (x2, y2, _) = sqrt $ ((x2 - x1) ** 2) + ((y2 - y1) ** 2)

-- | Returns a point representing one of the strings ends.
interpretPosition :: LinePosition -> Tangle -> Point
interpretPosition FirstA (aline, _) = head aline
interpretPosition LastA  (aline, _) = last aline
interpretPosition FirstB (_, bline) = head bline
interpretPosition LastB  (_, bline) = last bline

-- | Appends a new point at the beginning or the end of one of the strings.
addPoint :: LinePosition -> Tangle -> Point -> Tangle
addPoint FirstA (aline, bline) p = (p : aline,    bline)
addPoint LastA  (aline, bline) p = (aline ++ [p], bline)
addPoint FirstB (aline, bline) p = (aline,        p : bline)
addPoint LastB  (aline, bline) p = (aline,        bline ++ [p])

-- | Based on orientation returns a new point where strings overlap.
halfWay :: Orientation -> Z -> Double -> Point -> Point -> Point
halfWay North z by (x1, y1, _) (x2, y2, _) = ((x1 + x2) / 2, y1 + by / 2, z)
halfWay South z by (x1, y1, _) (x2, y2, _) = ((x1 + x2) / 2, y1 - by / 2, z)
halfWay West  z by (x1, y1, _) (x2, y2, _) = (x1 - by / 2, (y1 + y2) / 2, z)
halfWay East  z by (x1, y1, _) (x2, y2, _) = (x1 + by / 2, (y1 + y2) / 2, z)

-- | Function for rotating a tangle 90 degrees.
rotate :: Orientation -> Orientation
rotate East  = South
rotate North = East
rotate West  = North
rotate South = West

-- | Antirotation move is equivalent to three rotations.
antirotate :: Orientation -> Orientation
antirotate = rotate . rotate . rotate

-- | Generalised function for applying either twist or antitwist on a tangle. 
twist' :: Z -> TangleWithEdges -> Orientation -> TangleWithEdges 
twist' z (knot, (nw, ne, se, sw)) North = (twistTangle knot z nw ne North, (ne, nw, se, sw))
twist' z (knot, (nw, ne, se, sw)) East  = (twistTangle knot z ne se East,  (nw, se, ne, sw))
twist' z (knot, (nw, ne, se, sw)) South = (twistTangle knot z se sw South, (nw, ne, sw, se))
twist' z (knot, (nw, ne, se, sw)) West  = (twistTangle knot z sw nw West,  (sw, ne, se, nw))

-- | Twist is sliding lower string over the higher one.
twist :: TangleWithEdges -> Orientation -> TangleWithEdges
twist = twist' Over

-- | Antitwist is sliding lower string under the higher one. 
antitwist :: TangleWithEdges -> Orientation -> TangleWithEdges
antitwist = twist' Under


-- | Inverse of z coordinate of a point, used for creating two points at the intersection of strings in a twist/antitwist operation.
shift :: Z -> Z
shift Normal = Normal
shift Over = Under
shift Under = Over

-- | Creates four new points which expand the tangle after twist/antitwist operation.
create :: Orientation -> Z -> Double -> Point -> Point -> [Point]
create North z by p1 p2 = [halfWay North z by p1 p2, halfWay North (shift z) by p1 p2] ++ (map (.+ ( 0,  by, Normal)) [p1, p2])
create South z by p1 p2 = [halfWay South z by p1 p2, halfWay South (shift z) by p1 p2] ++ (map (.+ ( 0, -by, Normal)) [p1, p2])
create West  z by p1 p2 = [halfWay West  z by p1 p2, halfWay West  (shift z) by p1 p2] ++ (map (.+ (-by,  0, Normal)) [p1, p2])
create East  z by p1 p2 = [halfWay East  z by p1 p2, halfWay East  (shift z) by p1 p2] ++ (map (.+ ( by,  0, Normal)) [p1, p2])


-- | Applies twist operation by expanding the tangle with four new points.
twistTangle :: Tangle -> Z -> LinePosition -> LinePosition -> Orientation -> Tangle
twistTangle knot z pos1 pos2 dir = let p1 = (interpretPosition pos1 knot)
                                       p2 = (interpretPosition pos2 knot)
                                       newPoints = create dir z (sqrt (distance p1 p2))
                                                                p1
                                                                p2
                                    in foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                             [(pos2, head newPoints),
                                              (pos1, head $ tail newPoints),
                                              (pos2, head $ tail $ tail newPoints),
                                              (pos1, last newPoints)]

-- | Expands the final tangle with four new points that extend the string ends away from tangles. This adds a nice touch to the final drawing. 
expandTangle :: TangleWithEdges -> TangleWithEdges
expandTangle (knot, edges@(ne, nw, sw, se)) = let
                                                byx = sqrt $ distance (interpretPosition ne knot) (interpretPosition nw knot)
                                                byy = sqrt $ distance (interpretPosition ne knot) (interpretPosition se knot)
                                              in
                                                (foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                                       [(ne, (interpretPosition ne knot) .+ (-byx * 0.8,  byy * 0.5, Normal)),
                                                        (nw, (interpretPosition nw knot) .+ ( byx * 0.8,  byy * 0.5, Normal)),
                                                        (sw, (interpretPosition sw knot) .+ ( byx * 0.8, -byy * 0.5, Normal)),
                                                        (se, (interpretPosition se knot) .+ (-byx * 0.8, -byy * 0.5, Normal))],
                                            edges)

zeroTangle, emptyTangle :: TangleWithEdges
-- | ZeroTangle is just two horizontal parallel strings.
zeroTangle = (([(0, 1, Normal), (1, 1, Normal)],
             [(0, 0, Normal), (1, 0, Normal)]),
            (FirstA, LastA, LastB, FirstB))
-- | emptyTangle is just two strings represented as dots.
emptyTangle = (([(0, 1, Normal)],
              [(0, 0, Normal)]),
             (FirstA, LastA, LastB, FirstB))

-- | The main recursive procedure which generates two sets of control points from given sequence of moves.
generateTangle' :: [TangleMove] -> (TangleWithEdges, Orientation)
generateTangle' [] = (zeroTangle, East)
generateTangle' [Twist] = (twist emptyTangle East, East)
generateTangle' [Antitwist] = (antitwist emptyTangle East, East)
generateTangle' (i:is)  = case i of Twist      -> (twist currentTangle orientation, orientation)
                                    Antitwist  -> (antitwist currentTangle orientation, orientation)
                                    Rotate     -> (currentTangle, rotate orientation)
                                    Antirotate -> (currentTangle, antirotate orientation)
                                    where remainingTangle = generateTangle' is
                                          currentTangle = fst remainingTangle
                                          orientation = snd remainingTangle

-- | Output from Alexander is a series of moves, required to untangle a given rational tangle. 'reverseSteps' converts this series of moves from being destructive to constructive.
reverseSteps :: [TangleMove] -> [TangleMove]
reverseSteps = map (\s -> case s of 
                               Twist -> Antitwist
                               Antitwist -> Twist
                               Rotate -> Antirotate
                               Antirotate -> Rotate)

-- | Constructs a 'Knot' from given 'KnotMove's
generateTangle :: [TangleMove] -> (Tangle, Orientation)
generateTangle steps = (tangle, orientation)
                       where (tangleWithEdges, orientation) = generateTangle' steps
                             tangle = fst $ expandTangle $ tangleWithEdges


-- | 'renderTangle' takes a series of 'KnotMove's and a filename, into which it renders a SVG image representing the given tangle.
renderTangle' :: [TangleMove] -> String -> IO ()
renderTangle' moves filename =
  let
    (tangle, orientation) = generateTangle moves

    pointTo2D :: Point -> (Double, Double)
    pointTo2D (x, y, _) = (x, y)

    takeOvers _ []  = []
    takeOvers ((_, _, Over):ps) (l:ls) = l : takeOvers ps ls
    takeOvers (_:ps) (_:ls) = takeOvers ps ls

    oversData []  = []
    oversData (p1@(x1, y1, Over):p2@(x2, y2, _):ps) = (distance p1 p2) : oversData ps
    oversData (_:ps) = oversData ps

    lengths' tangle = map (\d -> max (d / 15.0) 0.5) $ oversData $ (fst tangle ++ snd tangle)

    splinesOver which f tangle = (explodeTrail $ cubicSpline False (map p2 $ map pointTo2D $ which tangle)) # takeOvers (f $ which tangle) #  mconcat
    splinesOver' which f tangle = (explodeTrail $ cubicSpline False (map p2 $ map pointTo2D $ which tangle)) # takeOvers (f $ which tangle) # zipWith lwL (lengths' tangle) #  mconcat

    lineStyle color = lc color # lwL 0.05

    lineWhole which tangle = cubicSpline False (map p2 $ map pointTo2D $ which tangle)

    blineDiaOverA = splinesOver fst id   tangle # lineStyle red
    blineDiaOverB = splinesOver fst tail tangle # lineStyle red
    alineDiaOverA = splinesOver snd id   tangle # lineStyle black
    alineDiaOverB = splinesOver snd tail tangle # lineStyle black
    blineDiaWhole = lineWhole fst tangle # lineStyle red
    alineDiaWhole = lineWhole snd tangle # lineStyle black

    alineDiaOverAWhite = splinesOver' snd id   tangle # lineStyle white
    alineDiaOverBWhite = splinesOver' snd tail tangle # lineStyle white
    blineDiaOverAWhite = splinesOver' fst id   tangle # lineStyle white
    blineDiaOverBWhite = splinesOver' fst tail tangle # lineStyle white

    getRotation :: Orientation -> Double
    getRotation South = 1/4
    getRotation West = 2/4
    getRotation North = 3/4
    getRotation East = 4/4

    diagram = (alineDiaOverA `atop` alineDiaOverB `atop`
              blineDiaOverA `atop` blineDiaOverB `atop`
              alineDiaOverAWhite `atop` alineDiaOverBWhite `atop`
              blineDiaOverAWhite `atop` blineDiaOverBWhite `atop`
              alineDiaWhole `atop` blineDiaWhole) # rotateBy (getRotation orientation)
  in
    renderSVG filename (mkSizeSpec (Just 400.0) (Just 400.0)) (diagram  # bg white)

-- | render Tangle via a set of moves required to untangle it
renderTangleReversed :: [TangleMove] -> String -> IO ()
renderTangleReversed = renderTangle' . reverseSteps

-- | render Tangle in a direct way
renderTangle :: [TangleMove] -> String -> IO ()
renderTangle = renderTangle' . reverse
