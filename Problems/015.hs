n `choose` k = fact n `div` (fact k * fact (n-k))
    where fact x = foldl (*) 1 [1..x]

answer = 40 `choose` 20
