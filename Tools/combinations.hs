n `comb` k = fact n `div` fact k * fact (n-k)
    where fact x = foldl (*) 1 [1..x]
