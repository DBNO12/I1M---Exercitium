import Data.Numbers.Primes

type Decimal = (Integer,[Integer],[Integer])
type Racional = (Integer,Integer)
data TipoDeRacional = Exacto | Puro | Mixto
     deriving (Show,Eq)

{- decimal (1,4)    ==  (0,[2,5],[])
   decimal (1,3)    ==  (0,[],[3])
   decimal (23,14)  ==  (1,[6],[4,2,8,5,7,1]) -}

decimal  :: Racional -> Decimal
decimal (p',q') | tipo == Exacto = (c, decimalesE r q, [])
                | tipo == Puro   = (c, [], periodo r q)
--                | p == Mixto  = (c, decimales r q, periodo r q)
  where (p,q) = irreducible (p',q')
        tipo  = clasifica (p,q)
        (c,r) = quotRem p q

-- decimalesE r q nos devuelve las cifras decimales de la división 
-- r/q, sabiendo que esta es de tipo exacta.
--      λ> decimales 4 5 = [8]
decimalesE :: Integer -> Integer -> [Integer]
decimalesE r q = [read[i] | i <- drop 2 $ show (f r/f q)]
  where f = fromIntegral

-- periodo r q nos devuelve el periodo de la división r/q
--      λ> periodo 1 3 == [3]
periodo :: Integer -> Integer -> [Integer]
periodo r q = esPeriodo 1 $ drop 2 $ show (f r/ f q)
  where f = fromIntegral

-- esPeriodo encuentra el periodo de las cifras decimales de una
-- división p/q (si esta no supera los 8 decimales)
esPeriodo :: Integer -> String -> [Integer]
esPeriodo 9 cs = error "El periodo es demasiago grande"
esPeriodo n cs | xs == take (f n) ys = [read [i] | i <- xs]
               | otherwise = esPeriodo (n+1) cs
  where (xs,ys) = splitAt (f n) cs
        f = fromIntegral

-- Nos devuelve el racional (p,q) de forma irreducible:
--      λ> irreducible (34,17) == (2,1)
irreducible :: Racional -> Racional
irreducible (p,q) = (p `div` m, q `div` m)
  where m = gcd p q

-- Clasifica el racional (p,q) según su expresión decimal
--      λ> clasifica (7,5)  == Exacto
--      λ> clasifica (7,15) == Mixto
--      λ> clasifica (7,21) == Puro
clasifica :: Racional -> TipoDeRacional
clasifica (p,q) | all (\x -> x == 2 || x == 5) ps = Exacto
                | 2 `elem` ps || 5 `elem` ps      = Mixto
                | otherwise                       = Puro
  where ps = primeFactors q
