import Data.Ratio
import Data.Char
import Control.Arrow

answer = fProduct . filter simpd $ [(a, b) | b <- [10..99], a <- [10..b]]
	where 
		simpd v@(a, b) = case l == f' of 
			   True -> if (l == '0' || l' == '0') || f == l then False
					   else ( (digitToInt f) % (digitToInt l') ) == (a % b)
			   _ 	-> False
			where 	
				a'@[f, l]   = show a
				b'@[f', l'] = show b
		fProduct xs = simplify (product $ map fst xs, product $ map snd xs)
			where 
				vmap f = f *** f 
				simplify (a, b) = vmap (flip div $ gcd a b ) (a, b)