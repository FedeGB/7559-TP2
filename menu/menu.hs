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
import Semaforo
import Sysadmin
import Log

chequearSalida salir = do
    valor <- Memoria.leer salir
    check (valor == 0) 

main = do
    archivoLog <- openFile "Logger.txt" WriteMode
    logger <- Log.crearLog
    semaforoCerraLog <- Semaforo.crearSemaforo 0
    forkIO ( Log.leer logger archivoLog semaforoCerraLog)
    salir <- Memoria.crear 0
    contenido <- readFile "config"

    let ancho = Parser.obtenerAncho contenido
    let alto = Parser.obtenerAlto contenido
    let maximoJugadores = Parser.obtenerMaximoJugadores contenido
    let cantidadDeJugadores = Parser.obtenerCantidadDeJugadores contenido
    let maximoDeConcumones = Parser.obtenerMaximoDeConcumones contenido
    let tiempoDeMovimiento = Parser.obtenerTiempoDeMovimientoConcumones contenido

    let posiciones = Grilla.generarGrilla ancho alto
    casillaOcupada <- CrearLista.crearLista (ancho*alto) [] 0

    let grillaAux = zip posiciones casillaOcupada
    let grilla = [(x,y,z) | ((x,y),z)<-grillaAux]

    nido <- forkIO (NidoDeConcumones.iniciarNido maximoDeConcumones tiempoDeMovimiento grilla logger)
    puntuacionesYJugadores <- AdministradorDeJugadores.cargarJugadores cantidadDeJugadores maximoJugadores grilla salir logger

    atomically(chequearSalida salir)

    Sysadmin.mostrarPuntuacionesFinales puntuacionesYJugadores logger

    Log.escribir logger "SALIR"

    Semaforo.p semaforoCerraLog

    putStrLn "Fin del juego"
