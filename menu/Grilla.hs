module Grilla
(
generarGrilla
) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Char
import Memoria

generarGrilla ancho alto = [(x,y)| x<-[1..ancho], y<-[1..alto]]