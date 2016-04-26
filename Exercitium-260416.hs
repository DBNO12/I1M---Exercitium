decimalAmultiple :: [Int] -> Int -> [Int]
decimalAmultiple ys x = tomaCoordenadas (base ys x 1 []) x
  where base :: [Int] -> Int -> Int -> [Int] -> [Int]
        base (y:ys) x n zs | x < n = zs
                           | otherwise = base ys x (y*n) (n:zs)
        tomaCoordenadas :: [Int] -> Int -> [Int]
        tomaCoordenadas []     _ = []
        tomaCoordenadas (y:ys) x = q: tomaCoordenadas ys r
            where (q,r) = divMod x y
                          
multipleAdecimal :: [Int] -> [Int] -> Int
multipleAdecimal xs = foldr1 (+) . zipWith (*) (1: scanl1 (*) xs) . reverse
