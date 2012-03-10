sieve [] primes = primes
sieve (x:xs) primes = x:sieve (filter (\n -> n `mod` x /= 0 ) xs) primes

primeList = sieve [2..1415] []

checkPrime n = foldl (\acc x -> if n `mod` x == 0 then False else acc) True primeList

answer = sum $ primeList ++ (filter checkPrime $ filter odd [2..2000000])
