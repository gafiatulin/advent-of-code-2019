-- https://adventofcode.com/2019/day/12

{-# LANGUAGE TupleSections #-}
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Function (fix)
import Data.List (foldl')
import Data.Maybe (isJust, catMaybes)

data Position = Position {_x, _y, _z ::Int} deriving (Show)
type Velocity = Position

initialState :: [(Position, Velocity)]
initialState = map (, Position{_x = 0, _y = 0, _z = 0}) undefined

ngramAndRest' :: [a] -> Int -> [a] -> [([a], [a])]
ngramAndRest' before n xs
    | n > length xs = []
    | otherwise = (gram, before ++ after) : ngramAndRest' (before ++ h) n rest
    where (gram, after) = splitAt n xs
          (h, rest) = splitAt 1 xs 

ngramAndRest :: Int -> [a] -> [([a], [a])]
ngramAndRest = ngramAndRest' []

energy :: [(Position, Velocity)] -> Int
energy = foldl' (\acc (p, v) -> acc + e' p * e' v) 0
    where e' Position{_x = x, _y = y, _z = z} = abs x + abs y  + abs z

step :: [(Position, Velocity)] -> [(Position, Velocity)]
step = concatMap (uncurry interact) . ngramAndRest 1
    where interact :: [(Position, Velocity)] -> [(Position, Velocity)] -> [(Position, Velocity)]
          interact current rest = map (\(p, v) -> (add p &&& id) . foldl' (\acc (p', _) -> add acc . gravity p $ p') v $ rest) current
          add Position{_x = x1, _y = y1, _z = z1} Position{_x = x2, _y = y2, _z = z2} = Position {_x = x1 + x2, _y = y1 + y2, _z = z1 + z2}
          gravity Position{_x = x1, _y = y1, _z = z1} Position{_x = x2, _y = y2, _z = z2} = Position{_x = g x1 x2, _y = g y1 y2, _z = g z1 z2}
          g = (signum .) . subtract

part1 = energy . foldl' (.) id (replicate 1000 step) $ initialState

part2 = (*2) . foldl' lcm 1 . catMaybes . fix(\rec state n (x, y, z) -> if all isJust [x, y, z] then [x, y, z] else let state' = step state in rec state' (succ n) (x <|> v0 n _x state', y <|> v0 n _y state', z <|> v0 n _z state')) initialState 1 $ (Nothing, Nothing, Nothing)
    where v0 n f ss = if all (== 0) . map (f . snd) $ ss then Just n else Nothing