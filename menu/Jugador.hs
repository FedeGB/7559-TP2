module Jugador
(
jugar
) where

import System.IO
import System.Random
import Control.Concurrent
import Data.Char

data Player = Player { id :: String
                     , turnos :: Int
                     , posicionInicial :: (Int,Int)
                     } deriving (Show)

obtenePosicionesValidas (x,y) grilla = [ a | a <- grilla, movimientoValido (x,y) a]

movimientoValido (x,y) (xf,yf)
    | (((abs $ x - xf) == 1) && ((abs $ y - yf) == 1)) = True
    | (((abs $ x - xf) == 1) && ((abs $ y - yf) == 0)) = True
    | (((abs $ x - xf) == 0) && ((abs $ y - yf) == 1)) = True
    | otherwise    = False

moverme (x,y) [] = (x,y)
moverme (x,y) ((x1,y1):xs)
    | movimientoValido (x,y) (x1,y1) = (x1,y1)
    | otherwise = moverme (x,y) xs

{-
Posicion de origen
grilla
cantidad de movimientos a realizar --> se lo reemplaza por memoria compartida con un comando de salida
id del jugador
-}
jugar _ _ 0 id = do
	putStrLn ("Jugador "++id++" termine de moverme")
jugar (x,y) grilla movimientos id = do
	{-Obtengo generadores aleatorios-}
	gen <- getStdGen
	gen2 <- newStdGen
	{-Obtengo la posicion a la que moverme-}
	let posicionesPosibles = obtenePosicionesValidas (x,y) grilla
	let maximoEspera = digitToInt('5')
	let maximo = (length (posicionesPosibles) - 1)
	let random = head (take 1 (randomRs (0,maximo) gen2))
	let espera = head (take 1 (randomRs (0,maximoEspera) gen))
	let posicionALaQueMeMuevo = (posicionesPosibles !! random)
	
	putStrLn $ "Jugador "++id ++" me movi de " ++ show (x,y) ++ " a " ++ show posicionALaQueMeMuevo

	threadDelay (100000*espera)

	jugar posicionALaQueMeMuevo grilla (movimientos-1) id