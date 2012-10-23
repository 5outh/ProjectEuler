import Data.Numbers.Primes
import Data.List
import Control.Monad

powerset = filterM (\_ -> [True, False])

factors = tail . init . nub . map product . powerset . primeFactors

abundants = filter (\x -> x < (sum $ factors x)) [2..14062]

isSumAbundant []     _    _ = False
isSumAbundant (a:as) prev x = (x - a) `elem` (a:prev) || isSumAbundant as (a:prev) x 

answer = filter notSum [1..28123]
  where notSum = not . isSumAbundant abundants []