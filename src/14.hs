-- https://adventofcode.com/2019/day/14

{-# LANGUAGE TupleSections #-}
import Data.Char (isSpace)
import Data.List (dropWhileEnd, foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Amount = Int
type Substance = String
type Ingridient = (Amount, Substance)
type Reaction = (Ingridient, [Ingridient])

reactions' = undefined

parseIngridient :: String -> Maybe Ingridient
parseIngridient = f . splitOn " "
    where f [a, s] = fmap (, s) . readMaybe $ a
          f _ = Nothing

parseReaction :: String -> Maybe Reaction
parseReaction = f . splitOn "=>"
    where f [inputs, output] = fmap (, mapMaybe (parseIngridient . trim) . splitOn "," $ inputs) . parseIngridient . trim $ output
          f _ = Nothing 
          trim = dropWhileEnd isSpace . dropWhile isSpace 

reactions :: Map.Map Substance (Amount, [Ingridient])
reactions = Map.insert "ORE" (1, []) . Map.fromList . map (\((a, s), is) -> (s, (a, is)) ) . mapMaybe parseReaction . lines $ reactions'

-- This is unsafe - Map.!
reduce :: Ingridient -> Map.Map Substance (Amount, [Ingridient]) -> Int
reduce (a, s) m = (Map.! "ORE") . foldl' reduce' (Map.singleton s a) . unroll m $ s
    where unroll m = let f ss s = if s `elem` ss then ss else s : foldl' (\b -> f b . snd) ss (snd . (Map.!) m $ s) in f []
          reduce' :: Map.Map Substance Amount -> Substance -> Map.Map Substance Amount 
          reduce' m' "ORE" = m'
          reduce' m' s' = foldl' (\mm (aa, ss) -> Map.insertWith (+) ss (aa * reactionN) mm) (Map.insert s' (reactionN * produced - need) m') ingridients
            where (produced, ingridients) = (Map.!) m s'
                  need = (Map.!) m' s'
                  reactionN = (\(d, m) -> if m == 0 then d else d + 1) $ need `quotRem` produced

part1 = reduce (1, "FUEL") reactions

-- This is unsafe - head
part2 = binSearch min' max' 
    where condition n = (<= 1000000000000) . reduce (n, "FUEL") $ reactions
          min' = 1000000000000 `quot` part1
          max' = head . dropWhile condition . map (min' *) $ [2..]
          binSearch a b = let m = (a + b) `quot` 2 in if b-a < 2 then maximum . filter condition $ [a..b] else if condition m then binSearch m b else binSearch a m
