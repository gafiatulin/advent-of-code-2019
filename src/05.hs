-- https://adventofcode.com/2019/day/5

import IntCodeComputer

diagnosticProgram :: Memory
diagnosticProgram = undefined

-- This is unsafe - head
result systemId = head . _output . runState . toState False [systemId] $ diagnosticProgram

part1 = result 1
part2 = result 5