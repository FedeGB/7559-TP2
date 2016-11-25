module Sysadmin
(
mostrarPuntuaciones
) where

import System.IO
import Control.Concurrent
import System.Random
import Data.Char
import Memoria

imprimirPuntuacion [] mensaje = do return mensaje
imprimirPuntuacion (x:xs) mensaje = do
    let puntuacion = (snd x)
    valor <- Memoria.leer2 puntuacion
    let puntuacion = "Jugador " ++ (show $ fst x) ++ " puntos: " ++ (show valor)++"\n"
    imprimirPuntuacion xs (mensaje ++ puntuacion)

mostrarPuntuaciones lista = do 
    gen <- getStdGen
    let espera = head (take 1 (randomRs (1,5) gen))
    threadDelay (1000000*espera)
    puntuaciones <- imprimirPuntuacion lista ""
    let mensaje = "\n********************\n" ++ "    PUNTUACIONES    \n" ++ puntuaciones ++ "********************\n"
    putStrLn mensaje
    mostrarPuntuaciones lista