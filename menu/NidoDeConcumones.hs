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
import Semaforo

chequearCrearConcumon concumonesActivos maxConcumones = do
    valor <- Memoria.leer concumonesActivos
    check (valor < maxConcumones) 

iniciarNido maxConcumones tMovimiento grilla = do
    concumonesActivos <- Memoria.crear 0
    semaforo <- Semaforo.crearSemaforo 1
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla semaforo

crearConcumon maxConcumones concumonesActivos tMovimiento grilla semaforo= do
    cId <- forkIO ( Concumon.iniciar grilla tMovimiento concumonesActivos semaforo)
    Memoria.escribir (\x -> x + 1) concumonesActivos
    threadDelay (1000000)
    atomically(chequearCrearConcumon concumonesActivos maxConcumones)
    crearConcumon maxConcumones concumonesActivos tMovimiento grilla semaforo