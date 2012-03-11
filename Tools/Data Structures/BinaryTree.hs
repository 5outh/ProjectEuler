module BinaryTree(
    Tree,
    singleton,
    insert,
    treeElem,
    toTree,
    nearest) where


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = singleton x
insert x (Node a left right)
    |x == a = Node x left right
    |x < a = Node a (insert x left) right
    |x > a = Node a left (insert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node a left right)
    |x==a = True
    |x < a = treeElem x left
    |x > a = treeElem x right

toTree :: (Ord a) => [a] -> Tree a
toTree xs = foldr insert Empty xs

nearest :: (Ord a) => a -> Tree a -> a
nearest _ Empty = undefined
nearest x (Node a left right)
    |x==a = x
    |x < a = if left == Empty then a else (nearest x left)
    |x > a = if right == Empty then a else (nearest x right)

