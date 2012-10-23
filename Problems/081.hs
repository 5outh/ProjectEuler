import Data.List.Split

data RDGraph = Target Int | Node Int RDGraph RDGraph | Empty deriving Show

matrix = unlines ["131,673,234,103,18",
				 "201,96,342,965,150",
				 "630,803,746,422,111",
				 "537,699,497,121,956",
				 "805,732,524,37,331"]
				 
numbers = map numsRow . map (splitOn ",") . lines				 
	where numsRow = map (\x -> read x :: Int)
	
toRDGraph :: [[Int]] -> RDGraph
toRDGraph xs = makeGraph 0 0 
	where
		makeGraph r c = if r >= length xs then Empty
					    else if c >= length (xs !! r) then Empty
						else if r == (length xs) -1 &&
								c == (length (xs !! r) -1)
								then Target ((xs !! r) !! c)
						else Node ((xs !! r) !! c) right down
							where
								right = makeGraph (succ r) c
								down  = makeGraph r (succ c)
									
minPath :: RDGraph -> Int
minPath (Target i) 	 = i
minPath (Node i r d) = i + min (minPath r) (minPath d)
minPath Empty 	     = 999999999

main = do
	file <- readFile "matrix.txt"
	let a = toRDGraph $ numbers file
	putStrLn $ show a