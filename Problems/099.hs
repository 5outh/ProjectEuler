import Data.List.Split

findMax :: [(Int, Integer)] -> (Int, Integer) ->  Int
findMax []     m = fst m
findMax (x@(a, b):xs) m@(c, d) = if b > d then findMax xs x else findMax xs m

readInt :: String -> Integer
readInt x = read x :: Integer

main = do
  contents <- readFile "base_exp.txt"
  let ints = zip [1..] (map toExp . map (splitOn ",") $ lines contents)
        where toExp [a, b] = (readInt a)^(readInt b)
  print $ findMax ints (0,0)