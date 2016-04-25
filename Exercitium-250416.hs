import Test.QuickCheck

productos :: Integer -> Integer -> [[Integer]]
productos n k = busca k [1..n] [n+1..]
  where busca :: Integer -> [Integer] -> [Integer] -> [[Integer]]
        busca k xs@(_:zs) (y:ys) | p > k     = []
                                 | p == k    = [xs]
                                 | otherwise = busca k (zs ++ [y]) ys
          where p = product xs

prop_prod :: (Positive Integer) -> (Positive Integer) -> Bool
prop_prod (Positive n) (Positive x) =
     productos n (product [x..x+n-1]) == [[x..x+n-1]]
--     λ> quickCheck prop_prod
--     +++ OK, passed 100 tests.

productosDe2y3consecutivos :: [Integer]
productosDe2y3consecutivos =
    [x | x <- [1..], (not . null) (productos 2 x),
                     (not . null) (productos 3 x)]
--     λ> head productosDe2y3consecutivos  ==  6
--     True


--     λ> take 2 (productosDe2y3consecutivos)
--     [6,210]

--     λ> take 3 (productosDe2y3consecutivos)
--     [6,210Interrupted.

prop_Mordell :: (Positive Integer) -> Bool
prop_Mordell (Positive 6)   = True
prop_Mordell (Positive 210) = True
prop_Mordell (Positive x)   =
      null (productos 2 x) || null (productos 3 x)
--     λ> quickCheck prop_Mordell
--     +++ OK, passed 100 tests.
