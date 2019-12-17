-- https://adventofcode.com/2019/day/1

import Data.Function (fix)
import Data.Monoid (getSum, Sum(..))

input :: [Integer]
input = undefined

fuel :: (Integral a) => a -> a
fuel m = m `quot` 3 - 2

fuel' :: (Integral a) => a -> a
fuel' m = fix(\rec x -> if x <= 0 then 0 else x + rec (fuel x)) m - m

part1 :: (Foldable t, Integral a) => t a -> a
part1 = getSum . foldMap (Sum . fuel)

part2 :: (Foldable t, Integral a) => t a -> a
part2 = getSum . foldMap (Sum . fuel')