primes x ps = if isPrime x ps then x:next (x:ps)
                         else next ps
              where next = primes (succ x)

isPrime x ps = foldl (\acc n -> if x `mod` n == 0 then False else acc) True poss
    where poss = filter (<= ceiling (sqrt (fromIntegral x) ) ) ps
