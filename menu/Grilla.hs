module Grilla
(
generarGrilla
) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Char
import Memoria


generarXY 1 yi 1 = (1,1,(Memoria.crear False)):[]
generarXY x yi 1 = (x,1,(Memoria.crear False)):(generarXY (x-1) yi yi)
generarXY x yi y = (x,y,(Memoria.crear False)):(generarXY x yi (y-1))

generarGrilla x y = reverse ( generarXY x y y)