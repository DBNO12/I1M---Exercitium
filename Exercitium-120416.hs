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
