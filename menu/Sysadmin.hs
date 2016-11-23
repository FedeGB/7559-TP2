module Sysadmin
(
mostrarPuntuaciones
) where

import System.IO
import Control.Concurrent
import System.Random
import Data.Char
import Memoria

imprimirPuntuacion [] = do return()
imprimirPuntuacion (x:xs) = do
    let puntuacion = (snd x)
    valor <- Memoria.leer2 puntuacion
    putStrLn $ "Jugador " ++ (show $ fst x) ++ " puntos: " ++ (show valor)
    imprimirPuntuacion xs

mostrarPuntuaciones lista = do 
    gen <- getStdGen
    let espera = head (take 1 (randomRs (1,5) gen))
    threadDelay (1000000*espera)
    putStrLn "********************"
    putStrLn "    PUNTUACIONES    "
    imprimirPuntuacion lista
    putStrLn "********************"
    mostrarPuntuaciones lista