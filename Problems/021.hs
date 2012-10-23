factors :: (Integral a) => a -> a -> [a]
factors n x
	| (fromIntegral n) > rt = []
	| (fromIntegral n) == rt = n:[]
	| otherwise = if x `mod` n == 0 then (x `quot` n):n:(factors (succ n) x )
				  else factors (succ n) x
	where rt = sqrt $ fromIntegral x

sumAmicables = foldr add [] sumDivs
	where 
		add (x , y) acc 
			| (x /= y) && (lookup y sumDivs) == Just x = (y:acc)
			| otherwise = acc
		propers x = 1 :  factors 2 x
		sumDivs = map (\x -> (x, sum $ propers x)) [1..10000]
