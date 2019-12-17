-- https://adventofcode.com/2019/day/9

import IntCodeComputer

boostProgram :: Memory
boostProgram = undefined

-- This is unsafe - head
result instruction = head . _output . runState . toState False [instruction] $ boostProgram

part1 = result 1
part2 = result 2