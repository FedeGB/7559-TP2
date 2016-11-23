import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Data.Char
import Data.List
import Parser
import Jugador
import Grilla
import Memoria
import AdministradorDeJugadores
import NidoDeConcumones
import Concumon

chequearSalida salir = do
    valor <- Memoria.leer salir
    check (valor == 0) 

main = do

    salir <- Memoria.crear 0

    contenido <- readFile "config"

    let ancho = Parser.obtenerAncho contenido
    let alto = Parser.obtenerAlto contenido
    let maximoJugadores = Parser.obtenerMaximoJugadores contenido
    let cantidadDeJugadores = Parser.obtenerCantidadDeJugadores contenido

    let grilla = Grilla.generarGrilla ancho alto

    AdministradorDeJugadores.cargarJugadores cantidadDeJugadores maximoJugadores grilla salir
--     nido <- forkIO (NidoDeConcumones.iniciarNido 5 2 grilla )

    atomically(chequearSalida salir)

    putStrLn "Fin del juego"
