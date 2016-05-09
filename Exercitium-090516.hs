import Data.Numbers.Primes

emparejables :: Integer -> Integer -> [[Integer]]
emparejables 1 m = [[x] | x <- takeWhile (< m) primes]
emparejables n m = pruebaParejas xss ps
  where ps  = takeWhile (< m) primes
        xss = emparejables (n-1) m

pruebaParejas :: [[Integer]] -> [Integer] -> [[Integer]]
pruebaParejas (xs:xss) ps = 
  [xs ++ [p] | p <- dropWhile (<= last xs) ps, buenaPareja xs p] ++ 
  pruebaParejas xss ps
pruebaParejas  _       _         = []

buenaPareja :: [Integer] -> Integer -> Bool
buenaPareja xs p = all isPrime [toNumber [x,y] | x <- ys, y <- ys, x /= y]
  where ys = p:xs
   -- *Main> buenaPareja [3,7,109] 673
   -- True

toNumber :: [Integer] -> Integer
toNumber [x,y] = x*10^n+y
  where n = length (show y)
   -- *Main> toNumber [123,4567]
   -- 1234567