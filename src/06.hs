-- https://adventofcode.com/2019/day/6

import Data.Tree
import Data.Bifunctor (second)
import Data.Monoid (getSum, Sum(..))

parseOrbits :: String -> [(String, String)]
parseOrbits = map ( second (drop 1) . span (/= ')')) . lines

treeFromRoot :: (Eq a) => [(a, a)] -> a -> Tree a
treeFromRoot os = unfoldTree (\label -> (label, map snd . filter ((== label) . fst) $ os))

countOrbits :: Tree a -> Int
countOrbits = foldr ((+) . snd) 0 . foldTree (\x xs -> ((x, 0):) . map (\(a, b) -> (a, b + 1)) . concat $ xs)

newtype Triple a = Triple {unwrap :: (a, a, a)}
instance Functor Triple where 
    fmap f (Triple (x, y, z)) = Triple (f x, f y, f z)
instance (Semigroup a) => Semigroup (Triple a) where
    Triple (a,b,c) <> Triple (a',b',c') = Triple (a<>a',b<>b',c<>c')
instance (Monoid a) => Monoid (Triple a) where
    mempty = Triple (mempty, mempty, mempty)

countTransfers :: (Eq a) => a -> a -> Tree a -> Maybe Int
countTransfers from to = (\(a, _, _) -> a) . unwrap . foldTree f
    where f v = Triple . next v . unwrap . fmap (fmap getSum) . foldMap (fmap (fmap Sum))
          next _ (Just r, _, _) = (Just r, Nothing, Nothing)
          next _ (_, Just l, Just r) = (Just $ l + r, Nothing, Nothing)
          next v (_, l, r) = (Nothing, if v == from then Just 0 else fmap succ l, if v == to then Just 0 else fmap succ r)