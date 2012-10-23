import Data.Char
digits = zip [1, 2, 3, 4, 5, 6, 7, 8, 9] [3, 3, 5, 4, 4, 3, 5, 5, 4] --1..9
normals = zip [0..] [0..]
teens  = zip [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] [3, 6, 6, 8, 8, 7, 7, 9, 8, 8] --10-19
tens   = zip [2, 3, 4, 5, 6, 7, 8, 9] [6, 6, 5, 5, 5, 7, 6, 6] --20..90
hundreds = map (\(a, b) -> (a, b+7)) digits --100..900

count :: Int -> Int
count = add . map digitToInt . show
	where 
		add (x:xs) = case length xs of
			3 -> 11
			2 -> case xs of
					[0, 0] -> extract (lookup x hundreds)
					_ 	   -> ( extract (lookup x hundreds) )  + 3 + (add xs)
			1 -> case x of
					0 -> add xs
					1 -> extract $ lookup (head xs) teens
					_ -> if xs == [0] then extract $ lookup x tens
						 else (extract $ lookup x tens) + add xs
			0 -> extract $ lookup x digits
			_ -> error "Number too long to parse"
		extract (Just a) = a
		extract Nothing  = 0

answer = sum . map count . enumFromTo 1

main = do 
	print $ answer 1000
	return ()