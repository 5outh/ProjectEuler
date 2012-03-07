module PrimeTools(
    primeFactors,
    factors,
    sieve
    ) where

primeFactors x d
    |x < d = [x]
    |x `mod` d == 0 = d: primeFactors (x `div` d) d
    |otherwise = primeFactors x (succ d)

factors x d
    |x <= d = [x]
    |x `mod` d == 0 = if x == d then [] else x:d: primeFactors (x `div` d) d
    |otherwise = primeFactors x (succ d)

sieve [] primes = primes
sieve (x:xs) primes = x:sieved
    where filtered = filter (\n -> n `mod` x /= 0 ) xs
          sieved = sieve filtered primes
