import Data.List
import Data.Numbers.Primes

esPrimoPermutable :: Integer -> Bool
esPrimoPermutable 2 = True
esPrimoPermutable n = all odd digs && all isPrime perms
  where digs = [read [i] | i <- show n]
        perms = map (foldr1 toNum) (permutations digs)
        toNum :: Integer -> Integer -> Integer
        toNum x 1 = 10*x+1
        toNum x y = x*10^(ceiling $ logBase 10 (fromIntegral y)) + y 

primosPermutables :: [Integer]
primosPermutables = filter esPrimoPermutable primes

