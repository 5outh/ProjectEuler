data BTree = Empty | Tree Int BTree BTree deriving Show

maxPath :: BTree -> Int
maxPath Empty			  = 0
maxPath tree@(Tree i l r) = i + (maximum paths)
	where 
		paths = [maxPath l, maxPath r]

toTree :: [[Int]] -> BTree
toTree xs = makeTree 0 0 
	where
		makeTree row column = if row >= length xs then Empty 
							  else if column >= length (xs !! row) then Empty
							  else Tree ((xs !! row) !! column) left right
								where
									left    = makeTree (row+1) column
									right   = makeTree (row+1) (column+1)
							
main = do
	file <- readFile "triangle.txt"
	let ls = map (map (\x -> read x :: Int) ) . map words $ lines file
	putStrLn (show . maxPath $ toTree ls)