import Data.Numbers.Primes

answer = sum . drop 4 . take 15 
         $ filter (\x -> all isPrime 
		 $ map readInt (truncateR (show x) ++ truncateL (show x))) 
		 primes

truncateL [] = []
truncateL xs = xs : truncateL (init xs)
truncateR [] = []
truncateR xs = xs : truncateR (tail xs)

readInt :: String -> Integer
readInt x = read x :: Integer