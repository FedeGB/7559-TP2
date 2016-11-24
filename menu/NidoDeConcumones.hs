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

chequearCrearConcumon concumonesActivos maxConcumones = do
    valor <- Memoria.leer concumonesActivos
    check (valor < maxConcumones) 

iniciarNido maxConcumones tMovimiento grilla = do
    concumonesActivos <- Memoria.crear 0
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla

crearConcumon maxConcumones concumonesActivos tMovimiento grilla = do
    cId <- forkIO ( Concumon.iniciar grilla tMovimiento concumonesActivos)
    Memoria.escribir (\x -> x + 1) concumonesActivos
    threadDelay (1000000)
    atomically(chequearCrearConcumon concumonesActivos maxConcumones)
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla