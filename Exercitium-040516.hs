import Data.List

periodo :: Eq a => [a] -> [a]
periodo xs = head ([as | n <- [k | k <- [1..j], m `mod` k == 0],
                         let (as,bs) = splitAt n xs,
                         let (cs,_) = splitAt n bs,
                         as == cs] ++ [xs])
  where m = length xs
        j = m `div` 2
