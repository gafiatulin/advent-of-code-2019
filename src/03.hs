-- https://adventofcode.com/2019/day/3

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap, first, second)
import Data.List (find, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (maybeToList, mapMaybe, listToMaybe)
import Data.Monoid (getSum, Sum(..))
import Data.Ord (comparing)
import Text.Read (readMaybe)

type Move = (Direction, Int)
type Point = (Int, Int)
type Range = Point
type Segment = (Point, Point)
data Direction = R | L | U | D deriving (Eq, Show, Read, Ord)

wire1 = segments undefined
wire2 = segments undefined

parseMove :: String -> Maybe (Direction, Int)
parseMove = (\(d, len) ->  liftA2 (,) (readMaybe d) (readMaybe len)) . splitAt 1

nextPoint :: Point -> Move -> Point
nextPoint p (R, len) = first (+ len) p
nextPoint p (L, len) = first (subtract len) p
nextPoint p (U, len) = second (+ len) p
nextPoint p (D, len) = second (subtract len) p

segments :: String -> [Segment]
segments wire = zip points (drop 1 points) 
    where points = scanl nextPoint (0, 0) . mapMaybe parseMove . splitOn "," $ wire

overlap :: Range -> Range -> Bool
overlap (x1, y1) (x2, y2) = max x1 x2 <= min y1 y2

enumerateSegment :: Segment -> [Point] 
enumerateSegment ((x1, y1), (x2, y2)) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 ..  max y1 y2]]

-- enumarating segments is a really inefficient way to find intersection points
intersect :: Segment -> Segment -> [Point]
intersect (s1a, s1b) (s2a, s2b)
    | s1a > s1b = (s1b, s1a) `intersect` (s2a, s2b)
    | s2a > s2b = (s1a, s1b) `intersect` (s2b, s2a)
    | overlap (fst s1a, fst s1b) (fst s2a, fst s2b) && overlap (snd s1a, snd s1b) (snd s2a, snd s2b) = [p1 | p1 <- enumerateSegment (s1a, s1b), p2 <- enumerateSegment (s2a, s2b), p1 == p2]
    | otherwise = []

nonTrivialIntersections :: [Segment] -> [Segment] -> [Point]
nonTrivialIntersections ss1 ss2 = filter (/= (0, 0)) . foldMap (uncurry intersect) $ [(a, b) | a <- ss1, b <- ss2]

closestNonZeroIntersection :: [Segment] -> [Segment] -> Maybe Point 
closestNonZeroIntersection ss1 = listToMaybe . sortOn (\(x, y) -> abs x + abs y) . nonTrivialIntersections ss1

within :: Point -> Segment -> Bool
within (x, y) ((x1, y1), (x2, y2)) = (min x1 x2 <= x) && (x <= max x1 x2) && (min y1 y2 <= y) && (y <= max y1 y2)

segmentLength :: Segment -> Int 
segmentLength ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)  

stepsToIntersection :: Point -> [Segment] -> Int
stepsToIntersection intersection = 
    uncurry (+) . bimap (getSum . foldMap ( Sum . segmentLength)) (maybe 0 (curry segmentLength intersection . fst) . listToMaybe) . break (within intersection)

sumStepsToClosestIntersection :: [Segment] -> [Segment] -> Maybe Int
sumStepsToClosestIntersection ss1 ss2 = foldl (flip minMaybe) Nothing intersections
    where intersections = nonTrivialIntersections ss1 ss2
          minMaybe intersection = let s = stepsToIntersection intersection ss1 + stepsToIntersection intersection ss2 in Just . maybe s (min s)