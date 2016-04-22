import qualified Data.Matrix as M
import qualified Data.Vector as V

type Horario = M.Matrix Bool

cursosConflictivos :: Horario -> [Int] -> Bool
cursosConflictivos p is =  foldr1 (||) $ foldr1 (zipWith (&&)) xss
  where xss = map (\i -> V.toList $ M.getRow i p) is

ejHorarios1 :: Horario
ejHorarios1 = M.fromLists [[True,  True,  False, False],
                           [False, True,  True,  False],
                           [False, False, True,  True]]
