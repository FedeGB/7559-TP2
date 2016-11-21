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
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla False

crearConcumon maxConcumones concumonesActivos tMovimiento grilla True = do
    threadDelay (100000)
    let activos = Memoria.leer concumonesActivos
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla (maxConcumones == activos)

crearConcumon maxConcumones concumonesActivos tMovimiento grilla False = do
    cId <- forkIO ( Concumon.iniciar grilla tMovimiento concumonesActivos)
    let activos = Memoria.leer concumonesActivos
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla (maxConcumones == activos)