import Data.Maybe
import Data.Char
import Data.List

soluciones :: [String] -> String -> [[String]]
soluciones fs ls = soluciona [([],ls,fs)]
                   
soluciona :: [([String],String,[String])] -> [[String]]
soluciona ((ps,(q:qs),rs):xs)
    | isNothing e = soluciona xs
    | otherwise   =
        soluciona (xs ++ [(ys:ps, qs, delete ys rs) |
                                        ys <- fromJust e])
  where e = encuentra q rs
soluciona ((ps,[],_):xs) = reverse ps : soluciona xs
soluciona   _            = []
   -- Cada terna representa:
   --     (Fichas usadas en orden invero,
   --         letras restantes que buscar,
   --            batería de fichas donde buscar)
                           
encuentra :: Char -> [String] -> Maybe [String]
encuentra x rs | null xs   = Nothing
               | otherwise = Just xs
  where xs = filter (pertenece x) rs
   -- λ> encuentra 'x' ["HO","LA"]  λ> encuentra 'A' ["HO","LA"]
   -- Nothing                       Just ["LA"]
             
pertenece :: Char -> String -> Bool
pertenece x xs = xm `elem` xs || xM `elem` xs
  where xm = toLower x
        xM = toUpper x
   -- λ> pertenece 'h' "HOLA"  λ> pertenece 'h' "7OLA"
   -- True                     False
