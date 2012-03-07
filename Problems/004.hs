main = do
    putStrLn $ show maxTwo

reverseArray :: Int -> [Int]
reverseArray 0 = []
reverseArray x = (x `mod` 10) : reverseArray (x `div` 10)

reverseInt n = foldl1 (\acc x-> acc*10 + x) $ reverseArray n

products 900 = []
products x = (zipWith (*) [x, x-1..900] (repeat x)): products (x-1)

maxPalindrome = maximum $ filter (\n -> n == reverseInt n) (concat $ products 999)
maxTwo = head $ dropWhile (\n -> n /= reverseInt n) (concat $ products 999)
