module Semaforo
(
crearSemaforo,
p,
v
) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM

chequearSemaforoEnUno semaforo = do
    valor <- readTVar semaforo
    check (valor == 1) 

crearSemaforo valorInicial = do atomically (newTVar valorInicial)

v semaforo = do
    atomically ( do writeTVar semaforo 1 )

p semaforo = do
    atomically ( do chequearSemaforoEnUno semaforo
                    writeTVar semaforo 0 )