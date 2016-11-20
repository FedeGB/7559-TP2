module MovimientosUtils
(
obtenePosicionesValidas,
obtenerSoloCoordenadas,
movimientoValido,
moverme
)where

obtenePosicionesValidas (x,y) grilla = [ a | a <- grilla, movimientoValido (x,y) a]

obtenerSoloCoordenadas (x,y,_) = do
    (x,y)

movimientoValido (x,y) (xf,yf,_)
    | (((abs $ x - xf) == 1) && ((abs $ y - yf) == 1)) = True
    | (((abs $ x - xf) == 1) && ((abs $ y - yf) == 0)) = True
    | (((abs $ x - xf) == 0) && ((abs $ y - yf) == 1)) = True
    | otherwise    = False

moverme (x,y) [] = (x,y)
moverme (x,y) ((x1,y1):xs)
    | movimientoValido (x,y) (x1,y1,False) = (x1,y1)
    | otherwise = moverme (x,y) xs
