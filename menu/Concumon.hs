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



data Concumon = Concumon { id :: String
                     , posicionInicial :: (Int,Int)
                     } deriving (Show)


obtenerPosicionesSinConcumon grilla = [ a | a <- grilla, casillaValidaParaConcumon a]

casillaValidaParaConcumon (_,_,booleano)
    | (Memoria.leer2 booleano) == 0 = False
    | otherwise = True

consultarValorActualCasilla (x,y) (xf,yf,z)
    | x == xf && y == yf = (Memoria.leer2 z) /= 0
    | otherwise = False

concumonFueAtrapado (x,y) grilla = [ a | a <- grilla , consultarValorActualCasilla (x,y) a]

actualizarValoresCeldas (_,_,zo) (_,_,zf) = do
    Memoria.escribir (\x -> 0) zo
    Memoria.escribir (\x -> 1) zf

obtenerCeldaDeLista grilla (x,y) = head [ val | val <- grilla , filtroPos (x,y) val ]

filtroPos (x,y) (xf,yf,_)
    | x == xf && y == yf = True
    | otherwise = False

moverse _ _ _ concumonesActivos True = do
    (Memoria.escribir (\x -> x - 1 ) concumonesActivos)
    return ()

moverse (x,y) grilla tMovimiento concumonesActivos False = do
    gen <- getStdGen
    gen2 <- newStdGen
    {-Obtengo la posicion a la que moverme-}
    let posicionesValidasConcumon = obtenerPosicionesSinConcumon grilla
    let posicionesPosibles = obtenePosicionesValidas (x,y) posicionesValidasConcumon
    let maximo = (length (posicionesPosibles) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let celdaFinal = (posicionesPosibles !! random)
    let posicionALaQueMeMuevo = obtenerSoloCoordenadas celdaFinal
    actualizarValoresCeldas (obtenerCeldaDeLista grilla (x,y)) celdaFinal
    putStrLn $ "Concumon se movio de " ++ show (x,y) ++ " a " ++ show posicionALaQueMeMuevo

    threadDelay (100000*tMovimiento)
    moverse posicionALaQueMeMuevo grilla tMovimiento concumonesActivos (length (concumonFueAtrapado (x,y) grilla) == 1)

iniciar grilla tMovimiento concumonesActivos = do
    putStrLn $ "Concumon nacio del nido"
    Memoria.escribir (\x -> x + 1) concumonesActivos
    gen <- getStdGen
    gen2 <- newStdGen
    let posicionesValidasConcumon = obtenerPosicionesSinConcumon grilla
    let maximo = (length (posicionesValidasConcumon) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let posicionInicial = obtenerSoloCoordenadas (posicionesValidasConcumon !! random)
    moverse posicionInicial grilla tMovimiento concumonesActivos False