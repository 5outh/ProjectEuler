import qualified Data.MemoCombinators as M
import Data.List

collatz = collatz'
	where collatz' n
		| n == 1 = 1
		| n `mod` 2 == 0 = 1 + collatz (n `quot` 2)
		| otherwise = 1 +  collatz (3 * n + 1)
		
main = do 
	print . (elemIndex $ maximum collatzes) $ collatzes
	where collatzes = map collatz [1..999999]