import Data.List

data Direccion = N | S | E | O deriving (Show, Eq)
type Camino = [Direccion]

reducido :: Camino -> Camino
reducido = foldr eliminaContrario []

eliminaContrario :: Direccion -> Camino -> Camino
eliminaContrario N (S:xs) = xs
eliminaContrario S (N:xs) = xs
eliminaContrario E (O:xs) = xs
eliminaContrario O (E:xs) = xs
eliminaContrario x  xs    = x:xs

{- reducido []                              ==  []
   reducido [N]                             ==  [N]
   reducido [N,O]                           ==  [N,O]
   reducido [N,O,E]                         ==  [N]
   reducido [N,O,E,S]                       ==  [] 
   reducido [N,O,S,E]                       ==  [N,O,S,E]
   reducido [S,S,S,N,N,N]                   ==  []
   reducido [N,S,S,E,O,N]                   ==  []
   reducido [N,S,S,E,O,N,O]                 ==  [O]
   reducido (take (10^7) (cycle [N,E,O,S])) ==  []-}