module KdTree(
    KdTree,
    singleton,
    toKdTree,
    ordPoints
    )where

import Data.List

type Point = (Int, Int)
data KdTree p a = Empty | Node p a (KdTree p a) (KdTree p a) deriving (Show, Eq)

singleton :: Point -> a -> KdTree Point a
singleton (x, y) a = Node (x, y) a Empty Empty

toKdTree :: [Point] -> Int -> KdTree Point Int
toKdTree [] _ = Empty
toKdTree [point] a = singleton point (a `mod` 2)
toKdTree points a = Node cur axis (toKdTree lower $ succ a) (toKdTree higher $ succ a)
    where axis = a `mod` 2
          median = length (points) `div` 2
          splitSorted = splitAt median $ sortBy (ordPoints axis) points
          lower = fst splitSorted
          cur = head $ snd splitSorted
          higher = tail $ snd splitSorted

ordPoints :: (Ord a, Num a) => a -> Point -> Point -> Ordering
ordPoints axis p1 p2
    |fun p1 > fun p2 = GT
    |fun p1 < fun p2 = LT
    |otherwise = EQ
        where fun = if axis == 0 then fst else snd
