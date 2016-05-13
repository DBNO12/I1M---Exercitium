data Estado = Abierta | Cerrada 
  deriving (Eq, Show)

final :: Int -> [Estado]
final 0 = [Cerrada]
final n = Abierta: checkEstado 2 n
  where checkEstado :: Int -> Int -> [Estado]
        checkEstado m n 
            | m > n     = []
            | otherwise = paridad l : checkEstado (m+1) n
          where xs = filter (\x -> m `mod` x == 0) [2..n]
                l  = length xs
        paridad :: Int -> Estado
        paridad l | even l    = Abierta
                  | otherwise = Cerrada
  
