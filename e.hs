import Data.List
import Data.Function
import qualified Data.Map as M

soluciones :: [Int] -> [(Int,[Int])]
soluciones xs = nub $ busca [(0,xs)]
  where -- busca ::
        busca ((n,xs):ys)
          | esFinal xs = (n,xs): busca ys
          | otherwise  = busca (ys ++ zs)
          where zs = [(n+1,ws) | ws <- alicia xs]
        busca   _      = []
        -- esFinal ::
        esFinal (x:ys@(y:_)) = x <= y && esFinal ys
        esFinal  _           = True
        -- alicia ::
        alicia (x:ys@(y:xs)) 
          | x > y     = (y+1:x:xs):(x-1:x:xs):(map (x:) $ alicia ys)
          | otherwise = map (x:) $ alicia ys
        alicia  _     = []

finales :: [Int] -> [[Int]]
finales xs = nub $ busca' [xs]
  where -- busca' ::
        busca' (xs:yss)
          | esFinal xs = xs: busca' yss
          | otherwise  = busca' (yss ++ (filter (`notElem` yss) $ alicia xs))
        busca'  _      = []
        -- esFinal ::
        esFinal (x:ys@(y:_)) = x <= y && esFinal ys
        esFinal  _           = True
        -- alicia ::
        alicia (x:ys@(y:xs)) 
          | x > y     = (y+1:x:xs):(x-1:x:xs):(map (x:) $ alicia ys)
          | otherwise = map (x:) $ alicia ys
        alicia  _     = []




agrupa f = foldr (\(a,b) -> M.insertWith (++) a [b]) 
              M.empty . map (\x -> (f x, x))

--agrupa2 f xs = -- foldr (\(k,x) -> M.insert k x) M.empty ys
  --where ys = map (\x -> (f x, x)) $ group $ sortBy (compare `on` f) xs 
              
                                                        
