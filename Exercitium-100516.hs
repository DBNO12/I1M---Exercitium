sucFractal :: [Integer]
sucFractal =
    tail $ concat [[x,y] | (x,y) <- zip (0:sucFractal) [0..]]

sumaSucFractal :: Integer -> Integer
sumaSucFractal = sum . flip take sucFractal . fromIntegral
