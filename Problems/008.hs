import System.IO
import Data.Char

main = do
    contents <- readFile "largeNum.txt"
    let maxNum = findMax $ map digitToInt $ concat . lines $ contents
    putStrLn $ show maxNum

findMax :: [Int] -> Int
findMax [] = 0
findMax xs = max prod $ findMax $ tail xs
    where prod = product (take 5 xs)
