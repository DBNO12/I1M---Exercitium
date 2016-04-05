clasesEquivalencia :: [a] -> (a -> a -> Bool) -> [[a]]
clasesEquivalencia []     _ = []
clasesEquivalencia (x:xs) p = (x:[y | y <- xs, p x y]):
              clasesEquivalencia [z | z <- xs, not $ p x z] p

