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
import Log

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
    Memoria.escribirNoAtomicamente (\x -> 0) zo
    Memoria.escribirNoAtomicamente (\x -> 1) zf

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

moverse posicion grilla tMovimiento concumonesActivos True semaforo logger = do
    Log.escribir logger ( "Concumon fue atrapado en " ++ show (obtenerSoloCoordenadas posicion) )
    Memoria.escribir (\x -> x - 1 ) concumonesActivos
    return ()
moverse posicion grilla tMovimiento concumonesActivos False semaforo logger = do
    Semaforo.p semaforo
    posicionALaQueMeMuevo <- obtenerPosicion posicion grilla
    atrapado <- Memoria.leer2 (tercerElemento posicion)
    if ( atrapado == 0 )
        then do
            Memoria.escribir (\x -> 0) (tercerElemento posicion)
            Semaforo.v semaforo
            moverse posicion grilla tMovimiento concumonesActivos True semaforo logger
        else do
            atomically ( actualizarValoresCeldas posicion posicionALaQueMeMuevo )
            Semaforo.v semaforo
            let (xo,yo) = obtenerSoloCoordenadas posicion
            let (xf,yf) = obtenerSoloCoordenadas posicionALaQueMeMuevo
            Log.escribir logger ( "Concumon se movio de " ++ show (xo,yo) ++ " a " ++ show (xf,yf) )
            threadDelay (1000000*tMovimiento)
            moverse posicionALaQueMeMuevo grilla tMovimiento concumonesActivos False semaforo logger


iniciar grilla tMovimiento concumonesActivos semaforo logger = do
    gen <- getStdGen
    gen2 <- newStdGen

    Semaforo.p semaforo

    posicionesValidasConcumon <- obtener grilla []
    let maximo = (length (posicionesValidasConcumon) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let posicionInicial = (posicionesValidasConcumon !! random)
    let (x,y) = obtenerSoloCoordenadas posicionInicial
    atomically (do 
        let valorPosicion = tercerElemento (posicionesValidasConcumon !! random)
        Memoria.escribirNoAtomicamente (\x -> 1) valorPosicion
        )

    Semaforo.v semaforo

    Log.escribir logger ( "Concumon nacio del nido en " ++ (show (x,y)) )
    moverse posicionInicial grilla tMovimiento concumonesActivos False semaforo logger