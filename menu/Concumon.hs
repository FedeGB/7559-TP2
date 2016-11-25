module Concumon
(
iniciar
)where

import System.IO
import System.Random
import Control.Concurrent
import Control.Concurrent.STM
import Data.Char
import Memoria
import MovimientosUtils
import Semaforo

data Concumon = Concumon { id :: String
                     , posicionInicial :: (Int,Int)
                     } deriving (Show)


tercerElemento (x,y,z) = z

obtener [] posiciones = do return posiciones
obtener grilla posiciones = do
    let elemento = head grilla
    let movimientoValido = tercerElemento elemento
    booleano <- Memoria.leer2 movimientoValido
    if (booleano == 0)
        then obtener (tail grilla) (posiciones ++ [elemento])
        else obtener (tail grilla) posiciones


--obtenerPosicionesSinConcumon grilla = [ a | a <- grilla, casillaValidaParaConcumon a]

{-casillaValidaParaConcumon (_,_,booleano)
    | (Memoria.leer2 booleano) == 0 = True
    | otherwise = False
-}
consultarValorActualCasilla (x,y) (xf,yf,z)
    | x == xf && y == yf = (Memoria.leer2 z) /= 0
    | otherwise = False

--concumonFueAtrapado (x,y) grilla = [ a | a <- grilla , consultarValorActualCasilla (x,y) a]

actualizarValoresCeldas (_,_,zo) (_,_,zf) = do
    Memoria.escribir (\x -> 0) zo
    Memoria.escribir (\x -> 1) zf

obtenerCeldaDeLista grilla (x,y) = head [ val | val <- grilla , filtroPos (x,y) val ]

filtroPos (x,y) (xf,yf,_)
    | x == xf && y == yf = True
    | otherwise = False

moverse _ _ _ concumonesActivos True semaforo = do
    (Memoria.escribir (\x -> x - 1 ) concumonesActivos)
    return ()

moverse (x,y) grilla tMovimiento concumonesActivos False semaforo = do
    gen <- getStdGen
    gen2 <- newStdGen
    {-Obtengo la posicion a la que moverme-}
    --let posicionesValidasConcumon = obtenerPosicionesSinConcumon grilla
    posicionesValidasConcumon <- obtener grilla []

    Semaforo.p semaforo

    let posicionesPosibles = obtenePosicionesValidas (x,y) posicionesValidasConcumon
    {-Por si hay concumones que no se pueden mover-}
    if ((length posicionesPosibles) == 0 )
        then do
            Semaforo.v semaforo

            threadDelay (1000000*tMovimiento)
            atrapado <- Memoria.leer2 (tercerElemento ( obtenerCeldaDeLista grilla (x,y)))

            moverse (x,y) grilla tMovimiento concumonesActivos (atrapado == 0) semaforo

        else do
            let maximo = (length (posicionesPosibles) - 1)
            let random = head (take 1 (randomRs (0,maximo) gen2))
            let celdaFinal = (posicionesPosibles !! random)
            let posicionALaQueMeMuevo = obtenerSoloCoordenadas celdaFinal
            atomically ( do
                let celdaActual = (obtenerCeldaDeLista grilla (x,y))
                Memoria.escribirNoAtomicamente (\x -> 0) (tercerElemento celdaActual)
                Memoria.escribirNoAtomicamente (\x -> 1) (tercerElemento celdaFinal)
                )

            Semaforo.v semaforo

            putStrLn $ "Concumon se movio de " ++ show (x,y) ++ " a " ++ show posicionALaQueMeMuevo

            threadDelay (1000000*tMovimiento)

            atrapado <- Memoria.leer2 (tercerElemento ( obtenerCeldaDeLista grilla posicionALaQueMeMuevo) )

            moverse posicionALaQueMeMuevo grilla tMovimiento concumonesActivos (atrapado == 0) semaforo

iniciar grilla tMovimiento concumonesActivos semaforo = do
    gen <- getStdGen
    gen2 <- newStdGen
    pos <- obtener grilla []

    Semaforo.p semaforo

    posicionesValidasConcumon <- obtener grilla []-- = pos--obtenerPosicionesSinConcumon grilla
    let maximo = (length (posicionesValidasConcumon) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let posicionInicial = obtenerSoloCoordenadas (posicionesValidasConcumon !! random)
    atomically (do 
        let valorPosicion = tercerElemento (posicionesValidasConcumon !! random)
        Memoria.escribirNoAtomicamente (\x -> 1) valorPosicion
        )

    Semaforo.v semaforo
    --let valorPosicion = tercerElemento (posicionesValidasConcumon !! random)
    --Memoria.escribir (\x -> 1) valorPosicion
    putStrLn $ "Concumon nacio del nido en " ++ (show posicionInicial)
    moverse posicionInicial grilla tMovimiento concumonesActivos False semaforo