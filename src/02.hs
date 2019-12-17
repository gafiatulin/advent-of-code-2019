-- https://adventofcode.com/2019/day/2

import IntCodeComputer

gravityAssistProgram :: Memory
gravityAssistProgram = undefined

part1 p1 p2 = readCurrentPointerMemory . withPointer 0 . runState . toState False [] . (\(first3, rest) -> take 1 first3 ++ [p1, p2] ++ rest) . splitAt 3 $ gravityAssistProgram

-- This is unsafe - head
part2 expected = head [100 * noun + verb | noun <- [0..99], verb <- [0..99], part1 noun verb == expected]
