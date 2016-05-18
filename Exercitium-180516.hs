jarras :: (Int,Int,Int) -> [[(Int,Int)]]
jarras (a,b,c) = busca [[(0,0)]]
  where
    busca :: [[(Int,Int)]] -> [[(Int,Int)]]
    busca ((ys@((d,e):_)):xss)
      | d == c    = reverse ys : busca xss
      | otherwise = busca (xss ++
                           [(a,e):ys | d < a, (a,e) `notElem` ys] ++
                           [(d,b):ys | e < b, (d,b) `notElem` ys] ++
                           [(0,e):ys | d > 0, (0,e) `notElem` ys] ++
                           [(d,0):ys | e > 0, (d,0) `notElem` ys] ++
                           [(a,e-a+d):ys | d < 4, e > 0, d + e >a,
                                          (a,e-a+d) `notElem` ys] ++
                           [(d-b+e,b):ys | d > 0, e < 3, d + e > b,
                                          (d-b+e,b) `notElem` ys] ++
                           [(d+e,0):ys | e > 0, d + e <= a,
                                            (d+e,0) `notElem` ys] ++ 
                           [(0,d+e):ys | d > 0, d + e <= b,
                                            (0,d+e) `notElem` ys])
    busca [] = []
    
                           
