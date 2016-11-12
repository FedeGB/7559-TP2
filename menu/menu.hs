import System.IO
import Control.Concurrent
import System.Random
import Data.Char
import Data.List
import Parser
import Jugador
import Grilla

cargarJugadores 0 _ _ = do return 0
cargarJugadores cantidad id grilla = do

	gen <- getStdGen
	gen2 <- newStdGen

	let maximo = (length (grilla) - 1)
	let random = head (take 1 (randomRs (0,maximo) gen2))
	let posicionInicial = (grilla !! random)

	tid <- forkIO ( Jugador.jugar posicionInicial grilla 7 (show id))

	cargarJugadores (cantidad-1) (id+1) grilla

main = do

	contents <- readFile "config"

	let ancho = Parser.obtenerAncho contents
	let alto = Parser.obtenerAlto contents
	let maximoJugadores = Parser.obtenerMaximoJugadores contents

	let grilla = Grilla.generarGrilla ancho alto
	
	let idInicial = 0

	cargarJugadores maximoJugadores idInicial grilla

	threadDelay 10000000

