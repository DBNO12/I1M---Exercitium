sucFractal :: [Integer]
sucFractal = 0: fractal
  where fractal = 0: concat [[x,y] | (x,y) <- zip [1..] fractal]

sumaSucFractal :: Integer -> Integer
sumaSucFractal = sum . flip take sucFractal . fromIntegral

sucFractal2 :: [Integer]
sucFractal2 = 0: fractal
  where fractal = 0: (concat $ zipWith (\x y -> [x,y]) [1..] fractal)