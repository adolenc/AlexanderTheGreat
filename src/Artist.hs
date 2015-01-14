{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,GeneralizedNewtypeDeriving,NoMonomorphismRestriction #-}

-- | Module for drawing rational tangles.

module Artist
(
  TangleMove(..)
, generateTangle
, renderTangle
, renderTangleReversed
, renderTangleWithPoints 
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
data Z = Over | Under | Normal deriving (Eq, Show)

-- | Points are declared with x,y coordinate and a special z coordinate. They are used for constructing and shaping a tangle.
type Point = (Double, Double)
type Sections = [Z]
-- | Represents one of the strings. Each string is defined as a spline with points used as spline control points.
type Line = [Point]
-- | Tangle is represented as a pair of strings.
type Tangle = (Line, Line, Sections, Sections)
-- | Represents a tuple of both strings ends. The order tells us the current location of a string end. First position corresponds to NW direction, last one to SW position.
type EdgePoints = (LinePosition, LinePosition, LinePosition, LinePosition)
-- | Represents a knot and current positions of strings ends.
type TangleWithEdges = (Tangle, EdgePoints)

(.+) :: Point -> Point -> Point
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

-- | Euclidian distance between two points.
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ ((x2 - x1) ** 2) + ((y2 - y1) ** 2)

-- | Returns a point representing one of the strings ends.
interpretPosition :: LinePosition -> Tangle -> Point
interpretPosition FirstA (aline, _, _, _) = head aline
interpretPosition LastA  (aline, _, _, _) = last aline
interpretPosition FirstB (_, bline, _, _) = head bline
interpretPosition LastB  (_, bline, _, _) = last bline

-- | Appends a new point at the beginning or the end of one of the strings.
addPoint :: LinePosition -> Tangle -> Z -> Point -> Tangle
addPoint FirstA (aline, bline, asec, bsec) z p = (p : aline,    bline       , z : asec,    bsec)
addPoint LastA  (aline, bline, asec, bsec) z p = (aline ++ [p], bline       , asec ++ [z], bsec)
addPoint FirstB (aline, bline, asec, bsec) z p = (aline,        p : bline   , asec,        z : bsec)
addPoint LastB  (aline, bline, asec, bsec) z p = (aline,        bline ++ [p], asec,        bsec ++ [z])

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

-- | Creates two new points which expand the tangle after twist/antitwist operation.
create :: Orientation -> Double -> Point -> Point -> [Point]
create North by p1 p2 = map (.+ ( 0,  by)) [p1, p2]
create South by p1 p2 = map (.+ ( 0, -by)) [p1, p2]
create West  by p1 p2 = map (.+ (-by,  0)) [p1, p2]
create East  by p1 p2 = map (.+ ( by,  0)) [p1, p2]


-- | Applies twist operation by expanding the tangle with four new points.
twistTangle :: Tangle -> Z -> LinePosition -> LinePosition -> Orientation -> Tangle
twistTangle knot z pos1 pos2 dir = let p1 = (interpretPosition pos1 knot)
                                       p2 = (interpretPosition pos2 knot)
                                       newPoints = create dir (sqrt (distance p1 p2)) p1 p2
                                    in foldl (\knt (pos, pt, z) -> addPoint pos knt z pt) knot
                                             [(pos2, head newPoints, z),
                                              (pos1, last newPoints, (shift z))]

-- | Expands the final tangle with four new points that extend the string ends away from tangles. This adds a nice touch to the final drawing. 
expandTangle :: TangleWithEdges -> TangleWithEdges
expandTangle (knot, edges@(ne, nw, sw, se)) = let
                                                byx = sqrt $ distance (interpretPosition ne knot) (interpretPosition nw knot)
                                                byy = sqrt $ distance (interpretPosition ne knot) (interpretPosition se knot)
                                              in
                                                (foldl (\knt (pos, pt) -> addPoint pos knt Normal pt) knot
                                                       [(ne, (interpretPosition ne knot) .+ (-byx * 0.8,  byy * 0.5)),
                                                        (nw, (interpretPosition nw knot) .+ ( byx * 0.8,  byy * 0.5)),
                                                        (sw, (interpretPosition sw knot) .+ ( byx * 0.8, -byy * 0.5)),
                                                        (se, (interpretPosition se knot) .+ (-byx * 0.8, -byy * 0.5))],
                                                 edges)

zeroTangle, emptyTangle :: TangleWithEdges
-- | ZeroTangle is just two horizontal parallel strings.
zeroTangle = (([(0, 1), (1, 1)],
               [(0, 0), (1, 0)],
               [Normal], [Normal]),
             (FirstA, LastA, LastB, FirstB))
-- | emptyTangle is just two strings represented as dots.
emptyTangle = (([(0, 1)],
                [(0, 0)],
                [], []),
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
renderTangle' :: Bool -> [TangleMove] -> String -> IO ()
renderTangle' drawPoints moves filename =
  let
    ((aline, bline, asec, bsec), orientation) = generateTangle moves

    sectionsOver [] _  = []
    sectionsOver _ []  = []
    sectionsOver (Over:ss) (l:ls) = l : sectionsOver ss ls
    sectionsOver (_:ss) (_:ls) = sectionsOver ss ls

    oversDistance [] _ = []
    oversDistance _ [] = []
    oversDistance (p1:p2:ps) (Over:ss) = (distance p1 p2) : oversDistance (p2:ps) ss
    oversDistance (_:ps) (_:ss) = oversDistance ps ss

    lineOver line sections width = (explodeTrail $ cubicSpline False (map p2 line)) # sectionsOver sections # zipWith lwL widths #  mconcat
                                   where widths = case width of
                                                     Just w -> repeat w
                                                     Nothing -> map (\d -> max (d / 15.0) 0.3) $ (oversDistance line sections)
    lineWhole = cubicSpline False . map p2

    lineStyle color = lc color # lwL 0.05


    redWhole = lineWhole aline # lineStyle red
    blkWhole = lineWhole bline # lineStyle black
    redMask = lineOver aline asec Nothing # lineStyle white
    blkMask = lineOver bline bsec Nothing # lineStyle white
    redOver = lineOver aline asec (Just 0.05) # lineStyle red
    blkOver = lineOver bline bsec (Just 0.05) # lineStyle black

    getRotation :: Orientation -> Double
    getRotation South = 1/4
    getRotation West = 2/4
    getRotation North = 3/4
    getRotation East = 4/4

    points = if drawPoints
             then position (zip (map p2 aline) (repeat (circle 0.1 # fc darkblue # lw none))) `atop`
                  position (zip (map p2 bline) (repeat (circle 0.1 # fc darkblue # lw none)))
             else mempty

    diagram = (points `atop`
               blkOver `atop` redOver `atop`
               blkMask `atop` redMask `atop`
               redWhole `atop` blkWhole) # rotateBy (getRotation orientation)
  in
    renderSVG filename (mkSizeSpec (Just 400.0) (Just 400.0)) (diagram  # bg white)

-- | render Tangle via a set of moves required to untangle it
renderTangleReversed :: [TangleMove] -> String -> IO ()
renderTangleReversed = renderTangle' False . reverseSteps

-- | render Tangle in a direct way, with points
renderTangleWithPoints :: [TangleMove] -> String -> IO ()
renderTangleWithPoints = renderTangle' True . reverse

-- | render Tangle in a direct way
renderTangle :: [TangleMove] -> String -> IO ()
renderTangle = renderTangle' False . reverse
