sieve [] primes = primes
sieve (x:xs) primes = x:sieve (filter (\n -> n `mod` x /= 0 ) xs) primes

answer = last $ take 10001 $ sieve [2..] []
