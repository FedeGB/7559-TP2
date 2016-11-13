import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Data.Char
import Data.List
import Parser
import Jugador
import Grilla

chequearSalida salir = do
    valor <- readTVar salir
    check (valor == 0)

cargarJugadores 0 _ _ _ = do return 0
cargarJugadores cantidad id grilla salir = do

    gen <- getStdGen
    gen2 <- newStdGen

    let maximo = (length (grilla) - 1)
    let random = head (take 1 (randomRs (0,maximo) gen2))
    let posicionInicial = (grilla !! random)

    atomically( do valor <- readTVar salir
                   writeTVar salir (valor + 1) )

    tid <- forkIO ( Jugador.jugar posicionInicial grilla 7 (show id) salir)

    cargarJugadores (cantidad-1) (id+1) grilla salir

main = do

	salir <- atomically (newTVar 0)

	contenido <- readFile "config"

	let ancho = Parser.obtenerAncho contenido
	let alto = Parser.obtenerAlto contenido
	let maximoJugadores = Parser.obtenerMaximoJugadores contenido

	let grilla = Grilla.generarGrilla ancho alto
	
	let idInicial = 0

	cargarJugadores maximoJugadores idInicial grilla salir

	atomically(chequearSalida salir)

	putStrLn "Fin del juego"

