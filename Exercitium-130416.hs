sumas  :: Int -> [[Int]]
sumas 0 = [[]]
sumas 1 = [[1]]
sumas n = map (1:) xs ++ map (2:) ys
  where xs = sumas (n-1)
        ys = sumas (n-2)

nSumas :: Int -> Integer
nSumas n = xs !! n
  where xs = 1:1: zipWith (+) xs (tail xs)