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
    | (Memoria.leer booleano) == 0 = False
    | otherwise = True

consultarValorActualCasilla (x,y) (xf,yf,z)
    | x == xf && y == yf = (Memoria.leer z) /= 0
    | otherwise = False

concumonFueAtrapado (x,y) grilla = [ a | a <- grilla , consultarValorActualCasilla (x,y) a]

moverse (x,y) grilla tMovimiento
    | length (concumonFueAtrapado (x,y) grilla) == 1 = return()
    | otherwise = do
    gen <- getStdGen
    gen2 <- newStdGen
    {-Obtengo la posicion a la que moverme-}
    let posicionesValidasConcumon = obtenerPosicionesSinConcumon grilla
    let posicionesPosibles = obtenePosicionesValidas (x,y) posicionesValidasConcumon
    let maximoEspera = digitToInt('5')
    let maximo = (length (posicionesPosibles) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let espera = head (take 1 (randomRs (0,maximoEspera) gen))
    let posicionALaQueMeMuevo = obtenerSoloCoordenadas (posicionesPosibles !! random)

    putStrLn $ "Concumon se movio de " ++ show (x,y) ++ " a " ++ show posicionALaQueMeMuevo

    threadDelay (100000*tMovimiento)
    moverse posicionALaQueMeMuevo grilla tMovimiento

iniciar grilla tMovimiento concumonesActivos = do
    putStrLn $ "Concumon nacio del nido"

    gen <- getStdGen
    gen2 <- newStdGen
    let posicionesValidasConcumon = obtenerPosicionesSinConcumon grilla
    let maximo = (length (posicionesValidasConcumon) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let posicionInicial = obtenerSoloCoordenadas (posicionesValidasConcumon !! random)
    Memoria.escribir (\x -> x + 1) concumonesActivos
    moverse posicionInicial grilla tMovimiento