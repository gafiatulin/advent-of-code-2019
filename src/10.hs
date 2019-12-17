-- https://adventofcode.com/2019/day/10

{-# LANGUAGE TupleSections #-}

import Data.List (maximumBy, sortBy)
import Control.Arrow ((&&&), second)
import Data.Map.Strict (toList, fromListWith)
import Data.Ord (comparing)

asteroidMap :: String 
asteroidMap = undefined

nthElement :: Int -> [a] -> Maybe a
nthElement i xs = if i < 0 then Nothing else it xs i
    where it :: [a] -> Int -> Maybe a
          it (x:_) 0 = Just x
          it (_:ys) j = it ys (pred j)
          it [] _ = Nothing

counts :: (Ord a) => [a] -> [(a, Int)]   
counts = toList . fromListWith (+) . map (,1)

asteroidLocations :: String -> [(Int, Int)]
asteroidLocations = concatMap (\(y, line) -> map((, y) . fst) . filter ((== '#') . snd) . zip [0..] $ line) . zip [0..] . lines

linesOfSight :: (Int, Int) -> [(Int, Int)] -> [((Int, Int), Int)]
linesOfSight (x', y') = counts . concatMap (uncurry delta')
    where delta' x y = let dx = x - x'
                           dy = y - y'
                           gcd' = gcd dx dy
                       in [(dx `quot` gcd', dy `quot` gcd') | gcd' /= 0]

nthDestroyed n lineOfSightCount
    | length lineOfSightCount < n = nthDestroyed (n - length lineOfSightCount) (filter ((> 0) . snd) . map (second pred) $ lineOfSightCount)
    | otherwise = nthElement (pred n) . sortBy (\(a, _) (b, _) -> counterClockwiseOrder a b) $ lineOfSightCount 
    where counterClockwiseOrder (x1, y1) (x2, y2) = compare (atan2 (fromIntegral x2) (fromIntegral y2)) (atan2 (fromIntegral x1) (fromIntegral y1))

asteroids = asteroidLocations asteroidMap

-- This is unsafe - maximumBy 
stationLocationWithLineOfSightCount = maximumBy (comparing (fst . snd)) . (\ls -> map(id &&& (length &&& id) . (`linesOfSight` ls)) ls) $ asteroids

part1 = fst. snd $ stationLocationWithLineOfSightCount

part2 = fmap (\(x, y) -> 100 * x + y) $ nthDestroyed 200 linesOfSight >>= uncurry select
    where ((x', y'), (_, linesOfSight)) = stationLocationWithLineOfSightCount
          select :: (Int, Int) -> Int -> Maybe (Int, Int)
          select (dx, dy) remaining = nthElement (pred remaining) . sortBy (flip (comparing distance)) . filter (\(x, y) -> atan2 (fromIntegral (x - x')) (fromIntegral (y - y')) == atan2 (fromIntegral dx) (fromIntegral dy) ) $ asteroids
          distance (x, y) = abs (x - x') + abs (y - y')