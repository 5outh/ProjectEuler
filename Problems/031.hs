{- 
   start with 200
  *simultaneously subtract 1, 2, 5, 10, 20, 50, 100, and 200
   if n > 0 , repeat
   if n < 0 , False
   if n == 0, True
   filter (== True) final
-}

combos 0 = [True]
combos n = concatMap combos next
  where next = filter (>=0) $ map (n-) [1, 2, 5, 10, 20, 50, 100, 200]