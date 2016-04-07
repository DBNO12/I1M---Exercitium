lucas :: [Integer]
lucas = 2:1: zipWith (+) ys xs
  where ys @ (_:xs) = lucas
                      
nLucas :: Integer -> Integer
nLucas n = head $ drop (fromIntegral n) lucas
