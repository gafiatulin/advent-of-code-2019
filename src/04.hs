-- https://adventofcode.com/2019/day/4

import Control.Arrow ((&&&))
import Data.List (intersect, nub)

condition1 :: String -> Bool
condition1 s = uncurry (&&) . (all (uncurry (<=)) &&& any (uncurry (==))) . zip s . drop 1  $ s

ngramAndRest' :: [a] -> Int -> [a] -> [([a], [a])]
ngramAndRest' before n xs
    | n > length xs = []
    | otherwise = (gram, before ++ after) : ngramAndRest' (before ++ h) n rest
    where (gram, after) = splitAt n xs
          (h, rest) = splitAt 1 xs

ngramAndRest :: Int -> [a] -> [([a], [a])]
ngramAndRest = ngramAndRest' []

condition2 :: String -> Bool
condition2 s = notDecreasing && criteria
    where criteria = any (\(ng, rest) -> (&& (null $ intersect rest ng)) . (== 1) . length . nub $ ng ) . ngramAndRest 2 $ s
          notDecreasing = all (uncurry (<=)) $ zip s (drop 1 s)

matchesWithin :: (String -> Bool) -> Int -> Int -> [String]
matchesWithin condition a b
    | b < a = matchesWithin condition b a
    | otherwise = filter condition . map show $ [a..b]