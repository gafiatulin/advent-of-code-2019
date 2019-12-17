-- https://adventofcode.com/2019/day/8

import Control.Arrow ((&&&))
import Data.List (minimumBy, foldl', intercalate)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

imgData :: String
imgData = undefined

image :: [Int]
image = map (read. (:[])) imgData

layers :: Int -> Int -> [Int] -> [[Int]]
layers w h = chunksOf (w*h)

-- This is unsafe - minimumBy
part1 w h min d1 d2 = uncurry (*) . (length . filter (== d1) &&& length . filter (== d2)) . minimumBy (comparing $ length . filter (== min)) . layers w h $ image

-- This is unsafe - non exhaustive match on colors
part2 w h = putStrLn rendered
    where rendered = intercalate "\n" . map (concatMap render) . chunksOf w . foldl' (zipWith colorMerge) (replicate (w * h) 2) . layers w h $ image
          colorMerge 2 x = x
          colorMerge x _ = x
          render 0 = "⬛️"
          render 1 = "⬜️"