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

actualizarValoresCeldas (_,_,zo) (_,_,zf) = do
    Memoria.escribir (\x -> 0) zo
    Memoria.escribir (\x -> 1) zf

obtenerCeldaDeLista grilla (x,y) = head [ val | val <- grilla , filtroPos (x,y) val ]

filtroPos (x,y) (xf,yf,_)
    | x == xf && y == yf = True
    | otherwise = False

obtenerPosicion posicion grilla = do
    posicionesValidasConcumon <- obtener grilla []
    let posicionesPosibles = obtenePosicionesValidas (obtenerSoloCoordenadas posicion) posicionesValidasConcumon
    if ((length posicionesPosibles) == 0 )
        then do
            return posicion
        else do
            gen <- getStdGen
            gen2 <- newStdGen
            let maximo = (length (posicionesPosibles) - 1)
            let random = head (take 1 (randomRs (0,maximo) gen2))
            let celdaFinal = (posicionesPosibles !! random)
            return celdaFinal

moverse2 posicion grilla tMovimiento concumonesActivos True semaforo = do
    putStrLn $ "Concumon fue atrapado en " ++ show (obtenerSoloCoordenadas posicion)
    Memoria.escribir (\x -> x - 1 ) concumonesActivos
    return ()
moverse2 posicion grilla tMovimiento concumonesActivos False semaforo = do
    Semaforo.p semaforo
    posicionALaQueMeMuevo <- obtenerPosicion posicion grilla
    atrapado <- Memoria.leer2 (tercerElemento posicion)
    if ( atrapado == 0 )
        then do
            Memoria.escribir (\x -> 0) (tercerElemento posicion)
            Semaforo.v semaforo
            moverse2 posicion grilla tMovimiento concumonesActivos True semaforo
        else do
            actualizarValoresCeldas posicion posicionALaQueMeMuevo
            Semaforo.v semaforo
            let (xo,yo) = obtenerSoloCoordenadas posicion
            let (xf,yf) = obtenerSoloCoordenadas posicionALaQueMeMuevo
            putStrLn $ "Concumon se movio de " ++ show (xo,yo) ++ " a " ++ show (xf,yf)
            threadDelay (1000000*tMovimiento)
            moverse2 posicionALaQueMeMuevo grilla tMovimiento concumonesActivos False semaforo


{-moverse _ _ _ concumonesActivos True semaforo = do
    (Memoria.escribir (\x -> x - 1 ) concumonesActivos)
    return ()

moverse (x,y) grilla tMovimiento concumonesActivos False semaforo = do
    gen <- getStdGen
    gen2 <- newStdGen
    {-Obtengo la posicion a la que moverme-}
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
-}
iniciar grilla tMovimiento concumonesActivos semaforo = do
    gen <- getStdGen
    gen2 <- newStdGen

    Semaforo.p semaforo

    posicionesValidasConcumon <- obtener grilla []
    let maximo = (length (posicionesValidasConcumon) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    --let posicionInicial = obtenerSoloCoordenadas (posicionesValidasConcumon !! random)
    let posicionInicial = (posicionesValidasConcumon !! random)
    let (x,y) = obtenerSoloCoordenadas posicionInicial
    atomically (do 
        let valorPosicion = tercerElemento (posicionesValidasConcumon !! random)
        Memoria.escribirNoAtomicamente (\x -> 1) valorPosicion
        )

    Semaforo.v semaforo

    putStrLn $ "Concumon nacio del nido en " ++ (show (x,y))
    --moverse posicionInicial grilla tMovimiento concumonesActivos False semaforo
    moverse2 posicionInicial grilla tMovimiento concumonesActivos False semaforo