{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,GeneralizedNewtypeDeriving,NoMonomorphismRestriction #-}

-- TODO:
-- - it's tangle not knot
-- - tangle should be datatype with {lineA, lineB}
-- - is west left or right?

module Artist
(
  KnotMove(..)
, renderTangle
) where

import Diagrams.Prelude hiding (Point, Line, rotate, shift, distance)
import Diagrams.Backend.SVG

data KnotMove = Twist | Antitwist | Rotate | Antirotate deriving (Eq)

data Z = Over | Under | Normal deriving (Eq)
type Point = (Double, Double, Z)

(.+) :: Point -> Point -> Point
(x1, y1, z1) .+ (x2, y2, _) = (x1 + x2, y1 + y2, z1)

type Line = [Point]
type Knot = (Line, Line)
data Orientation = West | North | East | South deriving (Show)

data LinePosition = FirstA | LastA | FirstB | LastB deriving (Show)
type EdgePoints = (LinePosition, LinePosition, LinePosition, LinePosition)

type KnotWithEdges = (Knot, EdgePoints)

rotate :: Orientation -> Orientation
rotate East  = North
rotate North = West
rotate West  = South
rotate South = East

antirotate :: Orientation -> Orientation
antirotate dir = iterate rotate dir !! 3

twist' :: Z -> KnotWithEdges -> Orientation -> KnotWithEdges 
twist' z (knot, (nw, ne, se, sw)) North = (twistKnot knot z nw ne North, (ne, nw, se, sw))
twist' z (knot, (nw, ne, se, sw)) East  = (twistKnot knot z ne se East,  (nw, se, ne, sw))
twist' z (knot, (nw, ne, se, sw)) South = (twistKnot knot z se sw South, (nw, ne, sw, se))
twist' z (knot, (nw, ne, se, sw)) West  = (twistKnot knot z sw nw West,  (sw, ne, se, nw))

twist :: KnotWithEdges -> Orientation -> KnotWithEdges
twist = twist' Over

antitwist :: KnotWithEdges -> Orientation -> KnotWithEdges
antitwist = twist' Under

interpretPosition :: LinePosition -> Knot -> Point
interpretPosition FirstA (aline, _) = head aline
interpretPosition LastA  (aline, _) = last aline
interpretPosition FirstB (_, bline) = head bline
interpretPosition LastB  (_, bline) = last bline

addPoint :: LinePosition -> Knot -> Point -> Knot
addPoint FirstA (aline, bline) p = (p : aline,    bline)
addPoint LastA  (aline, bline) p = (aline ++ [p], bline)
addPoint FirstB (aline, bline) p = (aline,        p : bline)
addPoint LastB  (aline, bline) p = (aline,        bline ++ [p])

halfWay :: Orientation -> Z -> Double -> Point -> Point -> Point
halfWay North z by (x1, y1, _) (x2, y2, _) = ((x1 + x2) / 2, y1 + by / 2, z)
halfWay South z by (x1, y1, _) (x2, y2, _) = ((x1 + x2) / 2, y1 - by / 2, z)
halfWay West  z by (x1, y1, _) (x2, y2, _) = (x1 - by / 2, (y1 + y2) / 2, z)
halfWay East  z by (x1, y1, _) (x2, y2, _) = (x1 + by / 2, (y1 + y2) / 2, z)

shift :: Z -> Z
shift Normal = Normal
shift Over = Under
shift Under = Over

create :: Orientation -> Z -> Double -> Point -> Point -> [Point]
create North z by p1 p2 = [halfWay North z by p1 p2, halfWay North (shift z) by p1 p2] ++ (map (.+ ( 0,  by, Normal)) [p1, p2])
create South z by p1 p2 = [halfWay South z by p1 p2, halfWay South (shift z) by p1 p2] ++ (map (.+ ( 0, -by, Normal)) [p1, p2])
create West  z by p1 p2 = [halfWay West  z by p1 p2, halfWay West  (shift z) by p1 p2] ++ (map (.+ (-by,  0, Normal)) [p1, p2])
create East  z by p1 p2 = [halfWay East  z by p1 p2, halfWay East  (shift z) by p1 p2] ++ (map (.+ ( by,  0, Normal)) [p1, p2])

distance :: Point -> Point -> Double
distance (x1, y1, _) (x2, y2, _) = sqrt $ ((x2 - x1) ** 2) + ((y2 - y1) ** 2)

twistKnot :: Knot -> Z -> LinePosition -> LinePosition -> Orientation -> Knot
twistKnot knot z pos1 pos2 dir = let p1 = (interpretPosition pos1 knot)
                                     p2 = (interpretPosition pos2 knot)
                                     newPoints = create dir z (sqrt (distance p1 p2))
                                                              p1
                                                              p2
                                  in foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                           [(pos2, head newPoints),
                                            (pos1, head $ tail newPoints),
                                            (pos2, head $ tail $ tail newPoints),
                                            (pos1, last newPoints)]

expandKnot :: KnotWithEdges -> KnotWithEdges
expandKnot (knot, edges@(ne, nw, sw, se)) = (foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                                   [(ne, (interpretPosition ne knot) .+ (-2,  1.0, Normal)),
                                                    (nw, (interpretPosition nw knot) .+ ( 2,  1.0, Normal)),
                                                    (sw, (interpretPosition sw knot) .+ ( 2, -1.0, Normal)),
                                                    (se, (interpretPosition se knot) .+ (-2, -1.0, Normal))],
                                            edges)

zeroKnot, emptyknot :: KnotWithEdges
zeroKnot = (([(0, 1, Normal), (1, 1, Normal)],
             [(0, 0, Normal), (1, 0, Normal)]),
            (FirstA, LastA, LastB, FirstB))
emptyknot = (([(0, 1, Normal)],
              [(0, 0, Normal)]),
             (FirstA, LastA, LastB, FirstB))

generateKnot' :: [KnotMove] -> (KnotWithEdges, Orientation)
generateKnot' [] = (zeroKnot, East)
generateKnot' [Twist] = (twist emptyknot East, East)
generateKnot' [Antitwist] = (antitwist emptyknot East, East)
generateKnot' (i:is)  = case i of Twist      -> (twist currentKnot orientation, orientation)
                                  Antitwist  -> (antitwist currentKnot orientation, orientation)
                                  Rotate     -> (currentKnot, rotate orientation)
                                  Antirotate -> (currentKnot, antirotate orientation)
                                  where remainingKnot = generateKnot' is
                                        currentKnot = fst remainingKnot
                                        orientation = snd remainingKnot

generateKnot :: [KnotMove] -> Knot
generateKnot steps = fst $ fst $ generateKnot' $ reverse steps

pointTo2D :: Point -> (Double, Double)
pointTo2D (x, y, z) = (x, y)

takeOvers _ []  = []
takeOvers ((_, _, Over):ps) (l:ls) = l : takeOvers ps ls
takeOvers (_:ps) (_:ls) = takeOvers ps ls

oversData []  = []
oversData (p1@(x1, y1, Under):p2@(x2, y2, _):ps) = ((x1, y1), (atan2 (y2 - y1) (x2 - x1)), distance p1 p2) : oversData ps
oversData (_:ps) = oversData ps

lengths' tangle = map (\(_, _, d) -> max (d / 25.0) 0.3) $ oversData $ (fst tangle ++ snd tangle)
splinesOver which f tangle = (explodeTrail $ cubicSpline False (map p2 $ map pointTo2D $ which tangle)) # takeOvers (f $ which tangle) #  mconcat
splinesOver' which f tangle = (explodeTrail $ cubicSpline False (map p2 $ map pointTo2D $ which tangle)) # takeOvers (f $ which tangle) # zipWith lwL (lengths' tangle) #  mconcat

lineStyle color = lc color # lwL 0.05

renderTangle :: [KnotMove] -> String -> IO ()
renderTangle moves filename =
  let
    tangle = generateKnot moves
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

    diagram = alineDiaOverA `atop` alineDiaOverB `atop`
              blineDiaOverA `atop` blineDiaOverB `atop`
              alineDiaOverAWhite `atop` alineDiaOverBWhite `atop`
              blineDiaOverAWhite `atop` blineDiaOverBWhite `atop`
              alineDiaWhole `atop` blineDiaWhole
  in
    renderSVG filename (mkSizeSpec (Just 400.0) (Just 400.0)) (diagram # bg white)
