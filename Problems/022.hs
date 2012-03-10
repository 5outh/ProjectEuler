import Data.List

main = do
    contents <- readFile "names.txt"
    let names = sort $ parse $ filter (/= '"') contents
        scores = nameScores names 1
    putStrLn $ show scores

nameScores :: [String] -> Int -> Int
nameScores [] _ = 0
nameScores (x:xs) step = (nameScore x) * step  + nameScores (xs) (succ step)

nameScore :: [Char] -> Int
nameScore name = foldl (\acc x -> acc + (cScore x)) 0 name
    where scoreMap = zip ['A'..'Z'] [1..]
          cScore char = snd $ head $ filter (\(l, s) -> l == char) scoreMap

parse :: String -> [String]
parse [] = []
parse string = curVal : parse newRunner
    where curVal = takeWhile (/= ',') string
          newVals = (string \\ curVal)
          newRunner
            | length newVals == 0 = []
            | otherwise = tail newVals
