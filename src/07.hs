-- https://adventofcode.com/2019/day/7

import IntCodeComputer
import Data.List (foldl', permutations)

type Phase = Int

amplifierControllerSoftware :: Memory
amplifierControllerSoftware = undefined

loopUntilLastHalt :: Input -> [State] -> Output
loopUntilLastHalt input states = if all (== 99) lastOps then output else loop
    where lastOps = map readCurrentPointerMemory reversedStates
          (output, reversedStates) = foldl' step (input, []) states
          step :: (Input, [State]) -> State -> (Output, [State])
          step (i, acc) state = let result = runState (withInput (_input state ++ i) state) in (_output result, result: acc)
          loop = loopUntilLastHalt output (foldl' (\acc s -> withInput [] s : acc) [] reversedStates)

-- This is unsafe - head
singleRun :: Memory -> [Phase] -> Int
singleRun mem = head . loopUntilLastHalt [0] . map (\p -> toState True [p] mem)

-- This is unsafe - maximum
result = maximum . map (singleRun amplifierControllerSoftware) . permutations

part1 = result [0..4]
part2 = result [5..9]