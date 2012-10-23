import Data.Ratio

eFrac _ 0 = 2 % (1 + 1)
eFrac n t = n / n + (eFrac (succ n) (pred t))