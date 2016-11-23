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
import CrearLista

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

    let posiciones = Grilla.generarGrilla ancho alto
    casillaOcupada <- CrearLista.crearLista (ancho*alto) [] 0

    let grillaAux = zip posiciones casillaOcupada
    let grilla = [(x,y,z) | ((x,y),z)<-grillaAux]

    AdministradorDeJugadores.cargarJugadores cantidadDeJugadores maximoJugadores grilla salir
--     nido <- forkIO (NidoDeConcumones.iniciarNido 5 2 grilla )

    atomically(chequearSalida salir)

    putStrLn "Fin del juego"
