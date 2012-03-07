import Data.List

primeFactors x d
    |x < d = []
    |x `mod` d == 0 = d: primeFactors (x `div` d) d
    |otherwise = primeFactors x (succ d)

triangles = scanl1 (+) [1..]

count x xs = (length $ c, removed)
    where c = filter (\n -> n == x) xs
          removed = xs \\ c

counts [] = []
counts (p:ps) = (fst c):(counts $ snd c)
    where c = count p (p:ps)

primeCounts x = product $ map (+1) $ counts factors
    where factors = primeFactors x 2

answer = head $ dropWhile (\x -> primeCounts x < 500) triangles
