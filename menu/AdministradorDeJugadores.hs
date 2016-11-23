module AdministradorDeJugadores
(
cargarJugadores
) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Jugador
import Grilla
import Memoria
import Sysadmin

crearLista 0 lista valorInicial = do
    return lista
crearLista cantidad lista valorInicial = do
    valor <- Memoria.crear valorInicial
    crearLista (cantidad-1) (lista ++ [valor]) valorInicial

chequearAgregarJugador maximo salir = do
    valor <- Memoria.leer salir
    check (valor < maximo) 

esperarQueSeLibereElMaximoDeJugadores [] _ _ = do return ()

esperarQueSeLibereElMaximoDeJugadores lista salir maximo = do
    atomically ( chequearAgregarJugador maximo salir )
    let iniciar = (snd (head lista))
    Memoria.escribir (\a -> True) iniciar
    Memoria.escribir (\x -> x + 1) salir
    esperarQueSeLibereElMaximoDeJugadores (tail lista) salir maximo

cargarJugadoresQueEsperan [] _ _ _= do
    return ()
cargarJugadoresQueEsperan listaDeJugadores listaDePuntuaciones salir grilla= do
    let iniciar = (snd (head listaDeJugadores))
    let puntos = (snd (head listaDePuntuaciones))
    let idJugador = fst (head listaDePuntuaciones)
    tid <- forkIO ( Jugador.iniciar grilla (show idJugador) salir iniciar puntos)
    cargarJugadoresQueEsperan (tail listaDeJugadores) (tail listaDePuntuaciones) salir grilla

cargarJugadoresHabilitados2 0 _ _ _ = do
    return ()
cargarJugadoresHabilitados2 cantidad grilla salir listaDePuntuaciones = do
    iniciar <- Memoria.crear True
    let puntos = (snd (head listaDePuntuaciones))
    --Memoria.escribir (\x -> x +1) puntos
    let idJugador = fst (head listaDePuntuaciones)
    tid <- forkIO ( Jugador.iniciar grilla (show idJugador) salir iniciar puntos)
    Memoria.escribir (\x -> x + 1) salir
    cargarJugadoresHabilitados2 (cantidad-1) grilla salir (tail listaDePuntuaciones)

cargarJugadores cantidadDeJugadores maximoJugadores grilla salir = do
    puntuaciones <- crearLista cantidadDeJugadores [] 0
    let puntuacionesYJugadores = zip [0..(cantidadDeJugadores-1)] puntuaciones

    esperar <- crearLista (cantidadDeJugadores-maximoJugadores) [] False
    let jugadoresQueEsperan = zip [maximoJugadores..(cantidadDeJugadores-1)] esperar
    let listaDePuntuacionesAux = drop maximoJugadores puntuacionesYJugadores

    cargarJugadoresHabilitados2 maximoJugadores grilla salir puntuacionesYJugadores
    cargarJugadoresQueEsperan jugadoresQueEsperan listaDePuntuacionesAux salir grilla
    forkIO (Sysadmin.mostrarPuntuaciones puntuacionesYJugadores)
    esperarQueSeLibereElMaximoDeJugadores jugadoresQueEsperan salir maximoJugadores
    return ()