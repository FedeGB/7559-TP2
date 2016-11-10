import System.IO
import Control.Concurrent
import System.Random
import Data.Char
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

altoGrilla parametros = digitToInt( parametros !! 0)

anchoGrilla parametros = digitToInt( parametros !! 1)

cantiadadJugadores parametros = digitToInt( parametros !! 2)

main = do

	contents <- readFile "config"
	--putStrLn (show contents)
	--let todoTasks = lines contents
	--putStrLn (show(todoTasks ))

	let parametros = [ a | a <- contents, a /= '\n',a/= ' ']

	let grilla = Grilla.generarGrilla (altoGrilla parametros) (anchoGrilla parametros)
	let cantidadDeJugadores = (cantiadadJugadores parametros)
	let idInicial = 0

	cargarJugadores cantidadDeJugadores idInicial grilla

	threadDelay 10000000

