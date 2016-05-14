import Data.Numbers.Primes
import Data.List

data Estado = Abierta | Cerrada 
  deriving (Eq, Show)
 
final :: Int -> [Estado]
final 0 = [Cerrada]
final n = Abierta: checkEstado 2 n
  where checkEstado :: Int -> Int -> [Estado]
        checkEstado m n 
            | m > n     = []
            | otherwise = paridad l : checkEstado (m+1) n
          where l = product [length xs + 1 |
                             xs <- group $ primeFactors m]
        paridad :: Int -> Estado
        paridad l | even l    = Cerrada
                  | otherwise = Abierta

