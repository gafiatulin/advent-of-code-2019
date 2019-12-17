-- https://adventofcode.com/2019/day/15

{-# LANGUAGE TupleSections #-}
import IntCodeComputer
import Data.Either (lefts, rights)
import Data.List (foldl', minimumBy)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Tree as Tree

repairDroidSoftware :: Memory
repairDroidSoftware = undefined

data Search = Search{
    _state :: State,
    _lastMove :: Maybe Int,
    _current :: (Int, Int),
    _visited :: Set.Set (Int, Int)
} deriving (Show)

-- This is unsafe - non exhaustive match on second parameter
step :: (Int, Int) -> Int -> (Int, Int)
step (x, y) 1 = (x, y + 1)
step (x, y) 2 = (x, y - 1)
step (x, y) 3 = (x - 1, y)
step (x, y) 4 = (x + 1, y)

-- This is unsafe - non exhaustive match in reverse', minimumBy is safe because of emptiness check
runSearch :: Bool -> Search -> Either [Int] (Set.Set (Int, Int))
runSearch mode Search{_state = state, _lastMove = lastMove, _current = current, _visited = visited} = case listToMaybe . _output $ state of
    Just 0 -> if mode then Left [] else Right visited
    Just 2 -> if mode then Left . maybeToList $ lastMove else next moves
    _ -> next moves
    where moves = filter (\m -> maybe True (\lm -> lm /= reverse' m) lastMove && ((`Set.notMember` visited) . step current $ m)) [1..4]
          runWith m = runSearch mode Search{_state = runState . withInput [m] $ state, _lastMove = Just m, _current = step current m, _visited = Set.insert current visited}
          next = (if mode then nextLeft . lefts else Right . Set.unions . rights) . map runWith
          nextLeft = Left . (\mss -> if null mss then [] else (\ms -> maybe ms (:ms) lastMove) . minimumBy (comparing length) $ mss) . filter (not . null)          
          reverse' 1 = 2
          reverse' 2 = 1
          reverse' 3 = 4
          reverse' 4 = 3
          
initState = Search{_state = toState False [] repairDroidSoftware, _lastMove = Nothing, _current = (0, 0), _visited = Set.empty}
path = either id (const []) . runSearch True $ initState

part1 = length path
part2 = either (const Nothing) (Just . Tree.foldTree (\_ xs -> if null xs then 0 else 1 + maximum xs) . toTree (foldl' step (0, 0) path)) . runSearch False $ initState
    where toTree tip set = Tree.unfoldTree (\(t, s) -> let connected = Set.filter (isConnected t) . Set.delete tip $ s in (t, map (, Set.difference s connected) . Set.elems $ connected)) (tip, set)
          isConnected (x1, y1) (x2, y2) = (abs (x2 - x1) + abs (y2 - y1)) < 2