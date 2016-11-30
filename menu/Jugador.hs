module Jugador
(
iniciar,
jugar
) where

import System.IO
import System.Random
import Control.Concurrent
import Control.Concurrent.STM
import Data.Char
import Memoria
import MovimientosUtils
import Log

cantidadDeMovimientos = 10

data Player = Player { id :: String
                     , turnos :: Int
                     , posicionInicial :: (Int,Int)
                     } deriving (Show)

chequearInicioDelJuego iniciar = do
    valor <- Memoria.leer iniciar
    check (valor)

salirAntes movimientos valorRandom
    | valorRandom == 0 = 0
    | otherwise = movimientos - 1

obtenerSiHayConcumon (x,y,z) = z

cargar concumon puntos = do
    Memoria.escribirNoAtomicamente (\x -> 0) concumon
    Memoria.escribirNoAtomicamente (\x -> x + 1) puntos

cargarPuntacion hayConcumon puntos = do
    atomically ( do
        atrapeConcumon <- Memoria.leer hayConcumon
        if ( atrapeConcumon == 1)
        then
            cargar hayConcumon puntos
        else
            return ()
        )

jugar _ _ 0 id salir puntos logger = do
    Log.escribir logger ( "Jugador "++id++" termine de moverme" )
    Memoria.escribir (\x -> x - 1) salir

jugar (x,y) grilla movimientos id salir puntos logger = do
    {-Obtengo generadores aleatorios-}
    gen <- getStdGen
    gen2 <- newStdGen
    {-Obtengo la posicion a la que moverme-}
    let posicionesPosibles = obtenePosicionesValidas (x,y) grilla
    let maximoEspera = digitToInt('5')
    let maximo = (length (posicionesPosibles) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let espera = head (take 1 (randomRs (0,maximoEspera) gen))
    let posicionALaQueMeMuevo = obtenerSoloCoordenadas (posicionesPosibles !! random)
    let hayConcumon = obtenerSiHayConcumon (posicionesPosibles !! random)

    cargarPuntacion hayConcumon puntos

    --Si el numero random es cero sale antes
    let movimientosRestantes = salirAntes movimientos espera

    Log.escribir logger ( "Jugador "++id ++" me movi de " ++ show (x,y) ++ " a " ++ show posicionALaQueMeMuevo )

    threadDelay (1000000*espera)

    --Reviso antes de irme si hay un concumon
    cargarPuntacion hayConcumon puntos

    jugar posicionALaQueMeMuevo grilla movimientosRestantes id salir puntos logger


iniciar grilla id salir iniciar puntos logger = do

    Log.escribir logger ("Jugador "++id ++" esperando para iniciar...")

    atomically ( chequearInicioDelJuego iniciar )

    Log.escribir logger ("Jugador "++id ++" inicio el juego")

    gen <- getStdGen
    gen2 <- newStdGen

    let maximo = (length (grilla) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let posicionInicial = obtenerSoloCoordenadas (grilla !! random)

    jugar posicionInicial grilla cantidadDeMovimientos id salir puntos logger