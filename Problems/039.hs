import Data.List
triangles p = length $ [(x, y, z) | x <- [1..(p `div` 2)],
                                    y <- [x..(p `div` 2)], 
                                    let z = p - (x + y), 
                                    x^2 + y^2 == z^2]

answer 0 max p' = p'
answer p max p' = if len > max then answer (pred p) len p else answer (pred p) max p'
  where len = triangles p 