import Test.QuickCheck

factGen :: Integer -> Integer -> Integer -> Integer
factGen x y z = product [x,x-z..x-(y-1)*z]

fact :: Integer -> Integer
fact 0 = 1
fact x = product [1..x] 

prop :: (Positive Integer) -> Bool
prop (Positive x) = fact x == factGen x x 1

-- λ> quickCheck prop
-- +++ OK, passed 100 tests.
