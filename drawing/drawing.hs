{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,GeneralizedNewtypeDeriving,NoMonomorphismRestriction #-}


import Diagrams.Prelude (Diagram, R2, (#), p2, lc, red, blue, orange, cubicSpline, atop, lwL)
import Diagrams.Backend.SVG.CmdLine (mainWith, B)

type Point = (Double, Double)

(.+) :: Point -> Point -> Point
(x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)

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

twist :: KnotWithEdges -> Orientation -> KnotWithEdges
twist (knot, (nw, ne, se, sw)) North = (twistKnot knot nw ne North, (ne, nw, se, sw))
twist (knot, (nw, ne, se, sw)) East  = (twistKnot knot ne se East,  (nw, se, ne, sw))
twist (knot, (nw, ne, se, sw)) South = (twistKnot knot se sw South, (nw, ne, sw, se))
twist (knot, (nw, ne, se, sw)) West  = (twistKnot knot sw nw West,  (sw, ne, se, nw))

antitwist :: KnotWithEdges -> Orientation -> KnotWithEdges
antitwist = twist

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

create :: Orientation -> Point -> Point -> [Point]
create North p1 p2 = map (.+ ( 0,  1)) [p1, p2]
create South p1 p2 = map (.+ ( 0, -1)) [p1, p2]
create West  p1 p2 = map (.+ (-1,  0)) [p1, p2]
create East  p1 p2 = map (.+ ( 1,  0)) [p1, p2]

twistKnot :: Knot -> LinePosition -> LinePosition -> Orientation -> Knot
twistKnot knot pos1 pos2 dir = let newPoints = create dir (interpretPosition pos1 knot)
                                                          (interpretPosition pos2 knot)
                                in foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                            [(pos2, head newPoints),
                                             (pos1, last newPoints)]

expandKnot :: KnotWithEdges -> KnotWithEdges
expandKnot (knot, (ne, nw, sw, se)) = (foldl (\knt (pos, pt) -> addPoint pos knt pt) knot
                                            [(ne, (interpretPosition ne knot) .+ (-1,  0.5)),
                                             (nw, (interpretPosition nw knot) .+ ( 1,  0.5)),
                                             (sw, (interpretPosition sw knot) .+ ( 1, -0.5)),
                                             (se, (interpretPosition se knot) .+ (-1, -0.5))], (ne, nw, sw, se))

zeroKnot, emptyknot :: KnotWithEdges
zeroKnot = (([(0, 1), (1, 1)],
             [(0, 0), (1, 0)]),
            (FirstA, LastA, LastB, FirstB))
emptyknot = (([(0, 1)],
              [(0, 0)]),
             (FirstA, LastA, LastB, FirstB))

generateKnot' :: [String] -> (KnotWithEdges, Orientation)
generateKnot' [] = (zeroKnot, East)
generateKnot' ["twist"]     = (twist emptyknot East, East)
generateKnot' ["antitwist"] = (antitwist emptyknot East, East)
generateKnot' (i:is) 
    | i == "twist"      = (twist     currentKnot orientation, orientation)
    | i == "antitwist"  = (antitwist currentKnot orientation, orientation)
    | i == "rotate"     = (currentKnot,     rotate orientation)
    | i == "antirotate" = (currentKnot, antirotate orientation)
    | otherwise = error "aaa doushio ~~ このステップ分からない..."
    where remainingKnot = generateKnot' is
          currentKnot = fst remainingKnot
          orientation = snd remainingKnot

generateKnot :: [String] -> Knot
generateKnot steps = fst $ expandKnot $ fst $ generateKnot' $ reverse steps

sampleKnot = generateKnot ["antitwist", "twist", "rotate", "twist", "antitwist", "rotate", "antitwist", "rotate", "rotate", "twist", "twist"]

alineDia = cubicSpline False (map p2 $ fst sampleKnot) # lc blue # lwL 0.05
blineDia = cubicSpline False (map p2 $ snd sampleKnot) # lc red # lwL 0.05
clineDia = cubicSpline False (map p2 $ take 5 $ snd sampleKnot) # lc orange # lwL 0.05

main = mainWith (alineDia `atop` blineDia :: Diagram B R2)
