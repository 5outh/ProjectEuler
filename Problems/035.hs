import Data.Numbers.Primes
import Control.Monad
import Control.Monad.Instances

circles x = circles' x (length $ show x)
  where circles' _ 0 = []
        circles' x n = c : circles' c (pred n)
		  where c = circleInt x

circleInt x = read (transform x) :: Int
  where transform = liftM2 (:) last init . show
  
circular = all isPrime . circles

answer = length . filter circular $ takeWhile (<1000000) primes