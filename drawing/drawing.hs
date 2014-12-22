{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,GeneralizedNewtypeDeriving,NoMonomorphismRestriction #-}


import Diagrams.Prelude (Diagram, R2, (#), p2, lc, red, blue, orange, cubicSpline, atop, lwL, (<>), circle, position, fc, lw, centerXY, explodeTrail, at, origin, fromSegments, mconcat, reverseTrail, white, bg, black)
import Diagrams.Backend.SVG.CmdLine (mainWith, B)
import Data.Random.Extras

data KnotMove = Twist | Antitwist | Rotate | Antirotate deriving (Eq)

data Z = Over | Under | Normal deriving (Eq, Show)
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
twist' z (knot, edges@(nw, ne, se, sw)) North = (twistKnot knot z nw ne North, (ne, nw, se, sw))
twist' z (knot, edges@(nw, ne, se, sw)) East  = (twistKnot knot z ne se East,  (nw, se, ne, sw))
twist' z (knot, edges@(nw, ne, se, sw)) South = (twistKnot knot z se sw South, (nw, ne, sw, se))
twist' z (knot, edges@(nw, ne, se, sw)) West  = (twistKnot knot z sw nw West,  (sw, ne, se, nw))

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

halfWay :: Orientation -> Z -> Point -> Point -> Point
halfWay North z (x1, y1, _) (x2, y2, _) = ((x1 + x2) / 2, y1 + 0.5, z)
halfWay South z (x1, y1, _) (x2, y2, _) = ((x1 + x2) / 2, y1 - 0.5, z)
halfWay West  z (x1, y1, _) (x2, y2, _) = (x1 - 0.5, (y1 + y2) / 2, z)
halfWay East  z (x1, y1, _) (x2, y2, _) = (x1 + 0.5, (y1 + y2) / 2, z)

shift :: Z -> Z
shift Normal = Normal
shift Over = Under
shift Under = Over

create :: Orientation -> Z -> Point -> Point -> [Point]
create North z p1 p2 = [halfWay North z p1 p2, halfWay North (shift z) p1 p2] ++ (map (.+ ( 0,  1, Normal)) [p1, p2])
create South z p1 p2 = [halfWay South z p1 p2, halfWay South (shift z) p1 p2] ++ (map (.+ ( 0, -1, Normal)) [p1, p2])
create West  z p1 p2 = [halfWay West  z p1 p2, halfWay West  (shift z) p1 p2] ++ (map (.+ (-1,  0, Normal)) [p1, p2])
create East  z p1 p2 = [halfWay East  z p1 p2, halfWay East  (shift z) p1 p2] ++ (map (.+ ( 1,  0, Normal)) [p1, p2])

twistKnot :: Knot -> Z -> LinePosition -> LinePosition -> Orientation -> Knot
twistKnot knot z pos1 pos2 dir = let newPoints = create dir z (interpretPosition pos1 knot)
                                                              (interpretPosition pos2 knot)
                                  in foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                              [(pos2, head newPoints),
                                               (pos1, head $ tail newPoints),
                                               (pos2, head $ tail $ tail newPoints),
                                               (pos1, last newPoints)]

expandKnot :: KnotWithEdges -> KnotWithEdges
expandKnot (knot, edges@(ne, nw, sw, se)) = (foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                                   [(ne, (interpretPosition ne knot) .+ (-1,  0.5, Normal)),
                                                    (nw, (interpretPosition nw knot) .+ ( 1,  0.5, Normal)),
                                                    (sw, (interpretPosition sw knot) .+ ( 1, -0.5, Normal)),
                                                    (se, (interpretPosition se knot) .+ (-1, -0.5, Normal))],
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
generateKnot' [Twist]     = (twist emptyknot East, East)
generateKnot' [Antitwist] = (antitwist emptyknot East, East)
generateKnot' (i:is) 
    | i == Twist      = (twist     currentKnot orientation, orientation)
    | i == Antitwist  = (antitwist currentKnot orientation, orientation)
    | i == Rotate     = (currentKnot,     rotate orientation)
    | i == Antirotate = (currentKnot, antirotate orientation)
    where remainingKnot = generateKnot' is
          currentKnot = fst remainingKnot
          orientation = snd remainingKnot

generateKnot :: [KnotMove] -> Knot
generateKnot steps = fst $ expandKnot $ fst $ generateKnot' $ reverse steps

sampleKnot = generateKnot [Twist, Rotate, Antitwist, Rotate, Antitwist, Rotate, Antitwist, Rotate, Twist]

pointTo2D :: Point -> (Double, Double)
pointTo2D (x, y, z) = (x, y)

takeOvers _ []  = []
takeOvers ((_, _, Over):ps) (l:ls) = l : takeOvers ps ls
takeOvers (_:ps) (_:ls) = takeOvers ps ls

splinesOver which f = (explodeTrail $ cubicSpline False (map p2 $ map pointTo2D $ which sampleKnot)) # takeOvers (f $ which sampleKnot) #  mconcat

lineStyle color = lc color # lwL 0.05

lineWhole which = cubicSpline False (map p2 $ map pointTo2D $ which sampleKnot)
blineDiaOverA = splinesOver fst id # lineStyle red
blineDiaOverB = splinesOver fst tail # lineStyle red
blineDiaWhole = lineWhole fst # lineStyle red
alineDiaWhole = lineWhole snd # lineStyle black
alineDiaOverA = splinesOver snd id # lineStyle black
alineDiaOverB = splinesOver snd tail # lineStyle black

overs = map p2 $ map pointTo2D $ filter (\(_,_,z) -> z == Over) $ (fst sampleKnot ++ snd sampleKnot)
dotRed = circle 0.2 # fc white # lwL 0
circlesDia = position $ zip overs $ repeat dotRed

tangles = alineDiaOverA `atop` alineDiaOverB `atop`
          blineDiaOverA `atop` blineDiaOverB `atop`
          circlesDia `atop`
          alineDiaWhole `atop` blineDiaWhole


main = mainWith (tangles # bg white :: Diagram B R2)
