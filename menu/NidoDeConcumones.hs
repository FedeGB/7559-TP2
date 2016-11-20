module NidoDeConcumones
(
iniciarNido
) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Grilla
import Concumon
import Memoria

iniciarNido maxConcumones tMovimiento grilla = do
    concumonesActivos <- Memoria.crear 0
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla

crearConcumon maxConcumones concumonesActivos tMovimiento grilla
    | maxConcumones > Memoria.leer2 concumonesActivos = forkIO ( Concumon.iniciar grilla tMovimiento concumonesActivos)
    | otherwise = do
    threadDelay (100000)
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla