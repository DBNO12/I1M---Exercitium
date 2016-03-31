import Data.Map as M
import Data.List as L

frecuencias :: Ord a => [a] -> Map a Int
frecuencias [] = empty
frecuencias (x:xs) = insertWith (+) x 1 (frecuencias xs)
