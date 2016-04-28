import Data.Numbers.Primes
import Data.List

pandigitalesPrimos :: [Int]
pandigitalesPrimos = filter isPrime pandigitales
  where pandigitales :: [Int]
        pandigitales = creaPandigitales 7
        creaPandigitales :: Int -> [Int]
        creaPandigitales 0 = []
        creaPandigitales n =
            sortBy (flip compare) (map toNum (permutations [1..n]))
                       ++ creaPandigitales (n-1)
        toNum :: [Int] -> Int
        toNum xs = foldr (\(x,n) s -> x*10^n + s) 0 (zip xs [0..])
