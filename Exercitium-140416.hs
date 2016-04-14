import Test.QuickCheck

sumaDiagonales :: Integer -> Integer
sumaDiagonales 0 = 0
sumaDiagonales n | odd n     = ciclo 1 1 n 0
                 | otherwise = ciclo 2 1 n 0

ciclo :: Integer -> Integer -> Integer -> Integer -> Integer
ciclo 1 1 n s | n == 1    = 1
              | otherwise = ciclo 3 3 n (s + 1)
ciclo 2 1 n s | n == 2    = 10
              | otherwise = ciclo 4 7 n (s + 10)
ciclo m x n s | m == n    = s + suma 
              | otherwise = ciclo (m+2) a n (s + suma)
  where suma = 4*x + 6*(m-1)
        a    = x + 4*m - 2

prop :: (Positive Integer) -> Bool
prop (Positive n) | odd n     = x `elem` [1,5,7]
                  | otherwise = x `elem` [0,4,6]
  where x = sumaDiagonales n `mod` 10

-- *Main> quickCheck prop
-- +++ OK, passed 100 tests.