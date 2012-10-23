data Fraction = Frac Integer Integer deriving Show -- Numerator Denominator

sqrtTwo = (map rep [0..] !!)
	where 
		rep 0 = Frac 1 2
		rep x = (Frac 1 1) |\| ( (Frac 2 1) |+| (sqrtTwo $ pred x))

answer = length . filter moreInDenom . roots
	where 
		roots = map (\x -> simplify $ Frac 1 1 |+| sqrtTwo x) . enumFromTo 0
		moreInDenom (Frac a b) = (length $ show a) > (length $ show b)

(|+|) :: Fraction -> Fraction -> Fraction
(|+|) f1@(Frac a b) f2@(Frac c d) = Frac num denom
	where 
		denom = lcm b d
		num	  = a * (denom `div` b) + c * (denom `div` d)
		
(|\|) :: Fraction -> Fraction -> Fraction
(|\|) f1@(Frac a b) f2@(Frac c d) = Frac num denom
	where
		num = a * d
		denom = b * c
		
simplify :: Fraction -> Fraction
simplify f@(Frac a b) = Frac (a `div` factor) (b `div` factor)
	where factor = gcd a b
	