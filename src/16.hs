-- https://adventofcode.com/2019/day/16

import Data.Char (digitToInt, intToDigit)
import Data.Function (fix)
import Data.List (scanl')

signal :: [Int]
signal = undefined

pattern'  :: Int -> [Int]
pattern' n = drop 1 . cycle . concatMap (replicate n) $ [0, 1, 0, -1]

fft :: [Int] -> [Int]
fft = flip (zipWith (((abs . (`rem` 10) . sum) .) . zipWith (*)) . (replicate =<< length)) (map pattern' [1..])

fft' :: [Int] -> [Int]
fft' = scanl' (((abs . (`rem` 10)) .) . (+)) 0

result :: ([Int] -> [Int]) -> ([Int] -> [Int]) -> [Int] -> String
result f g = map intToDigit . take 8 . f . fix(\rec n s -> if n == 0 then s else rec (pred n) (g s)) 100

part1 = result id fft signal
part2 = result reverse fft' . reverse . drop (read . map intToDigit . take 7 $ signal) . concat . replicate 10000 $ signal