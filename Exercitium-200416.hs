ausente :: Integral a => [a] -> a
ausente ys @ (x:y:z:xs) = compara [x,x+m..] ys
  where m = min (y-x) (z-y)
        compara :: Integral a => [a] -> [a] -> a
        compara (x:xs) (y:ys) | x == y    = compara xs ys
                              | otherwise = x
ausente _ = error "La lista debe contener mas de dos elementos."
