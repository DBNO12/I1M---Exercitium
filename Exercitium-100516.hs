sucFractal :: [Integer]
sucFractal = 0: fractal
  where fractal = 0: concat [[x,y] | (x,y) <- zip [1..] fractal]

sumaSucFractal :: Integer -> Integer
sumaSucFractal = sum . flip take sucFractal . fromIntegral