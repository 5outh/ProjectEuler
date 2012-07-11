import Data.List

main = do
    contents <- readFile "numBlock.txt"
        let ls  = lines contents
        array   = foldl (\acc x-> acc ++ [map (\z -> read z :: Integer) $ words x]) [] ls
        answer  = show $ findMax array (0, 0)
    putStrLn answer

findMax xs (19,19) = 0
findMax xs (r, c) = maximum [max, findMax xs (nextR, nextC)]
    where fw    = if c > 16 then 0 else foldl (\acc n -> acc * (xs!!r!!(c+n)) ) 1 [0..3]
          down  = if r > 16 then 0 else foldl (\acc n -> acc * (xs!!(r+n)!!c) ) 1 [0..3]
          fwd   = if r > 16 || c > 16 then 0 else foldl (\acc n -> acc * (xs!!(r+n)!!(c+n)) ) 1 [0..3]
          bkd   = if c < 3 || r > 16 then 0 else foldl (\acc n -> acc * (xs!!(r+n)!!(c-n)) ) 1 [0..3]
          max   = maximum [down, fw, fwd, bkd]
          nextC = if c < 19 then succ c else 0
          nextR = if nextC == 0 then succ r else r
