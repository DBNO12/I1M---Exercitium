data Expr = V String
           | S Expr Expr
           | P Expr Expr

esTermino :: Expr -> Bool
esTermino (V _)     = True
esTermino (P e1 e2) = esTermino e1 && esTermino e2
esTermino _         = False

esNormal :: Expr -> Bool
esNormal (S e1 e2) = esTermino e1 && esTermino e2
esNormal e         = esTermino e