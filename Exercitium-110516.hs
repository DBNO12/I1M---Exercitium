densidad :: Integer -> Float
densidad n = (\(x,m) -> m/fromIntegral n) $
             head $ dropWhile (\(x,_) -> x <= n) xs
 where xs = zip ys [0..]
       ys = map read $ filter noMonotono $ map show [101..]
            
menorConDensidadMayor :: Float -> Integer
menorConDensidadMayor d = fst $ head $ dropWhile (\(_,x) -> x < d) xs
 where xs = zipWith (\x n -> (n,x/fromIntegral n)) [1..] ys
       ys = map read $ filter noMonotono $ map show [101..]
            
noMonotono :: String -> Bool
noMonotono = noMonAux False False
  where noMonAux b1 b2 (x:xs@(y:_))
                 | x < y = b2 || noMonAux True b2 xs
                 | x > y = b1 || noMonAux b1 True xs
                 | otherwise = noMonAux b1 b2 xs
        noMonAux _ _ _ = False
   -- λ> noMonotono "123"  λ> noMonotono "321"  λ> noMonotono "312"
   -- False                False                True
