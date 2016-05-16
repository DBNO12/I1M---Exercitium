duplicadora :: Integer -> [Integer]
duplicadora = reverse . takeWhile (/= 0) . iterate (`div` 2)

duplicadoraEE :: Integer -> [Integer]
duplicadoraEE m = busca [[1]]
  where busca :: [[Integer]] -> [Integer]
        busca (xs@(x:_):xss) 
            | x == m = reverse xs
            | x > m  = busca xss 
            | otherwise = busca ((xx:xs):(xx + 1:xs):xss) 
          where xx = 2 * x
