import Data.List (sort)

minimaDiferencia :: (Num a, Ord a) => [a] -> a
minimaDiferencia zs@(_:_:_) = buscaMinimo (z:ys) (z-y)
  where (y:z:ys) = sort zs
        buscaMinimo :: (Num a, Ord a) => [a] -> a -> a
        buscaMinimo (x:y:xs) m | x == y    = 0
                               | n <  m    = buscaMinimo (y:xs) n
                               | otherwise = buscaMinimo (y:xs) m
          where n = y - x
        buscaMinimo _        m = m
minimaDiferencia _  = error "La lista debe contener mas de un elemento"