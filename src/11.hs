-- https://adventofcode.com/2019/day/11

import IntCodeComputer
import Control.Arrow ((***), (&&&), (>>>), first, second)
import Data.Semigroup (Min(..), Max(..), getMin, getMax)
import Data.List (minimumBy, maximumBy, intercalate)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

paintingRobotSoftware :: Memory
paintingRobotSoftware = undefined

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight
type Position = (Int, Int)
type Color = Int

data Robot = Robot{
    _program :: State,
    _position :: Position,
    _direction :: Direction,
    _world :: Map.Map Position Color
}

initRobot :: Memory -> Color -> Robot
initRobot program color' = Robot{
    _program = toState False [] program,
    _position = (0, 0),
    _direction = DirectionDown,
    _world = Map.singleton (0, 0) color'
}

turn :: Direction -> Int ->  Direction
turn DirectionUp 0 = DirectionLeft
turn DirectionUp 1 = DirectionRight
turn DirectionDown 0 = DirectionRight
turn DirectionDown 1 = DirectionLeft
turn DirectionLeft 0 = DirectionDown
turn DirectionLeft 1 = DirectionUp
turn DirectionRight 0 = DirectionUp
turn DirectionRight 1 = DirectionDown

step1 :: Direction -> Position -> Position
step1 DirectionUp = second succ
step1 DirectionDown = second pred
step1 DirectionLeft = first pred
step1 DirectionRight = first succ

-- This is unsafe - non exhaustive match on output list
runRobot :: Robot -> Robot
runRobot r = if halt then newRobot else runRobot newRobot
    where position = _position r
          world = _world r
          program = _program r
          color = Map.findWithDefault 0 position world
          program' = runState . withInput [color] $ program
          (turn':color':_) = _output program'
          direction' = turn (_direction r) turn'
          halt = (== 99) . readCurrentPointerMemory $ program'
          newRobot = Robot{
            _program = program',
            _position = step1 direction' position,
            _direction = direction',
            _world = Map.insert position color' world
          }

-- This is unsafe - non exhaustive match on colors
renderWorld :: Map.Map Position Color -> String
renderWorld w = intercalate "\n" . map (\y -> reverse . concat $ [render . Map.findWithDefault 0 (x, y) $ w | x <- [minX .. maxX]]) $ [minY .. maxY]
    where ((minX, maxX), (minY, maxY)) = let minmax = foldMap (Min &&& Max) >>> getMin *** getMax in (minmax *** minmax) . unzip . Map.keys $ w
          render 0 = "⬛️"
          render 1 = "⬜️"

result = _world . runRobot . initRobot paintingRobotSoftware

part1 = Map.size . result $ 0
part2 = putStrLn . renderWorld . result $ 1