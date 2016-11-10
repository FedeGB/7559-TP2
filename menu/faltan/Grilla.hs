module Grilla
(
generarGrilla
) where

import MemoriaCompartida


invertirGrilla [] = []
invertirGrilla ((x1,y1,libre):xs) = invertirGrilla xs ++ [(x1,y1,libre)]

generarXY 1 yi 1 libre = (1,1,libre):[]
generarXY x yi 1 libre = (x,1,libre):(generarXY (x-1) yi yi libre)
generarXY x yi y libre = (x,y,libre):(generarXY x yi (y-1) libre)

{-libre : TVar Bool-}
generarGrilla x y libre = invertirGrilla ( generarXY x y y libre)