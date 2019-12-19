-- https://adventofcode.com/2019/day/17

{-# LANGUAGE TupleSections #-}
import IntCodeComputer
import Control.Arrow ((***), (&&&), first, second)
import Control.Applicative ((<|>), (<$>))
import Control.Monad ((>=>))
import Data.Char (chr, ord)
import Data.Function (fix)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.List (foldl', findIndices, intercalate, isInfixOf, find, stripPrefix, isPrefixOf)
import qualified Data.Set as Set

data Direction = U | D | L | R deriving (Eq)
data RobotState = RobotState{
    _direction :: Direction,
    _position :: (Int, Int)
}

asciiProgram :: Memory
asciiProgram = undefined

scaffoldingMap :: String
scaffoldingMap =  map chr . reverse . _output . runState . toState False [] $ asciiProgram

indexedLines :: [(Int, String)]
indexedLines = zip [0..] . filter (not . null) . lines $ scaffoldingMap

traversePath :: Set.Set (Int, Int) -> RobotState -> Maybe [String]
traversePath tiles = fix (\rec path st -> maybe (Just path) (\(moves, st') -> rec (path ++ moves) st') . step $ st) []
    where distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
          turn = (`lookup` [((U,L), "L"), ((U,R), "R"), ((L,U), "R"), ((L, D), "L"), ((R,U), "L"), ((R,D), "R"), ((D,L), "R"), ((D,R), "L")])
          step RobotState{_direction = d, _position = p} = 
              fmap (\((d', _), p') -> ((++ [show . distance p $ p']) . maybeToList . turn $ (d, d'), RobotState{_direction = d', _position = p'}))
              . listToMaybe 
              . filter (uncurry (&&) . ((/= d) . snd *** (/= p))) 
              . map (second (\f -> fix(\rec p' -> let p'' = f p' in if Set.notMember p'' tiles then p' else rec p'') p))
              $ [((R, L), first succ), ((L, R), first pred), ((U, D), second pred), ((D, U), second succ)]

part1 = foldl' sumIntersectionAlignments 0 . zip3 indexedLines (drop 1 indexedLines) . drop 2 $ indexedLines
    where sumIntersectionAlignments acc ((_, prevLine), (n, line), (_, nextLine)) = (+ acc) . sum . map (*n) . findIndices id . zipWith (&&) (intersect prevLine line nextLine) . (++ [False]) . (False:) . intersect line (drop 1 line) . drop 2 $ line
          intersect = zipWith3 (\p l n -> all (`elem` "#<>^v") [p, l, n])

part2 = maybeAsciiMovementLogic >>= \asciiMovementLogic -> listToMaybe . _output . runState . toState False (map ord asciiMovementLogic) . (2:) . drop 1 $ asciiProgram
    where maybeAsciiMovementLogic = compressed >>= \compressed' -> fmap (\(a, b, c) -> (++ "\n") . intercalate "\n" . (compressed' :) . map (intercalate ",") $ [a, b, c, ["n"]]) abc
          compressed = fullPath >>= \path -> abc >>= fmap (intercalate ",") . compress path
          abc = fullPath >>= \path -> maxRepeatingPrefix path >>= (\a -> stripPrefixes [a] path >>= (maxRepeatingPrefix >=> \b -> stripPrefixes [a, b] path >>= (maxRepeatingPrefix >=> (\c -> stripPrefixes [a, b, c] path >>= \s -> if null s then Just (a, b, c) else Nothing))))
          fullPath = initial >>= uncurry traversePath
          initial = (\(ts, r) -> fmap (\(d, p) -> (ts, RobotState{_direction = d, _position = p})) r) . foldl' readMap (Set.empty, Nothing) $ indexedLines
          readMap (tiles, robot) (y, line) = let indexed = zip [0..] line in (Set.union tiles . Set.fromList . map ((,y) . fst) . filter ((== '#') . snd) $ indexed , robot <|> (listToMaybe . mapMaybe (\(x, v) -> fmap (, (x, y)) . (`lookup` [('>', R), ('<', L), ('^', U), ('v', D)]) $ v ) $ indexed)) 
          maxRepeatingPrefix ss = fmap fst . find ( \(prefix, rest) -> uncurry (&&) . ((`isInfixOf` rest) &&& (< 20) . length . intercalate ",") $ prefix) . map (`splitAt` ss) $ [10, 9 .. 1]
          stripPrefixes ps = fix(\rec s -> if any (`isPrefixOf` s) ps then (>>= rec) . listToMaybe . mapMaybe (`stripPrefix` s) $ ps else Just s)
          compress path (a, b, c)
            | null path = Just []
            | a `isPrefixOf` path = ("A" :) <$> compress (drop (length a) path) (a, b, c)
            | b `isPrefixOf` path = ("B" :) <$> compress (drop (length b) path) (a, b, c)
            | c `isPrefixOf` path = ("C" :) <$> compress (drop (length c) path) (a, b, c)
            | otherwise = Nothing

          
