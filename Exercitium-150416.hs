import Data.Numbers.Primes

sumas :: Integer -> [[Integer]]
sumas n = map reverse (conteo n xs xs [] 0)
  where xs = takeWhile (< n) primes

conteo :: Integer -> [Integer] -> [Integer] -> [Integer] 
          -> Integer -> [[Integer]]
conteo n (x:xs) (y:ys) zs s 
    | k >  n =         conteo n    xs  xs    []  0
    | k == n = (y:zs): conteo n    xs  xs    []  0
    | k <  n =         conteo n (x:xs) ys (y:zs) k
  where k = y + s
conteo _  _      _     _   _  = []