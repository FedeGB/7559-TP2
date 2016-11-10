module Grilla
(
generarGrilla
) where


invertirGrilla [] = []
invertirGrilla ((x1,y1):xs) = invertirGrilla xs ++ [(x1,y1)]

generarXY 1 yi 1 = (1,1):[]
generarXY x yi 1 = (x,1):(generarXY (x-1) yi yi)
generarXY x yi y = (x,y):(generarXY x yi (y-1))

generarGrilla x y = invertirGrilla ( generarXY x y y)