-- https://adventofcode.com/2019/day/13

import IntCodeComputer
import Data.List.Split (chunksOf)

arcadeCabinetSoftware :: Memory
arcadeCabinetSoftware = undefined

everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n-1) xs of
    (y:ys) -> y : everyNth n ys
    [] -> []

-- This is unsafe - head, last
runGame :: State -> State
runGame current = if (== 99) . readCurrentPointerMemory $ current then current else runGame . runState . withInput [signum (getX 4 - getX 3)] $ current
    where gameState = chunksOf 3 . _output $ current
          getX id = last . head . filter ((== id) . head) $ gameState

-- This is unsafe - head
part1 = length . filter (==2) . everyNth 3 . reverse .  _output . runState . toState False [] . (1:) $ arcadeCabinetSoftware
part2 = head . _output . runGame . runState . toState False [] . (2:) $ arcadeCabinetSoftware