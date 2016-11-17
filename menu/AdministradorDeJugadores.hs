module AdministradorDeJugadores
(
cargarJugadoresHabilitados
) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Jugador
import Grilla
import Memoria

chequearAgregarJugador maximo salir = do
    valor <- Memoria.leer salir
    check (valor < maximo) 


esperarQueSeLibereElMaximoDeJugadores [] _ _ = do return ()

esperarQueSeLibereElMaximoDeJugadores lista salir maximo = do
    atomically ( chequearAgregarJugador maximo salir )
    let iniciar = head lista
    Memoria.escribir (\a -> True) iniciar
    Memoria.escribir (\x -> x + 1) salir
    esperarQueSeLibereElMaximoDeJugadores (tail lista) salir maximo


cargarJugadoresRestantes 0 lista _ _ salir maximo = do
    esperarQueSeLibereElMaximoDeJugadores lista salir maximo

cargarJugadoresRestantes cantidad lista id grilla salir maximo = do
    iniciar <- Memoria.crear False
    tid <- forkIO ( Jugador.iniciar grilla (show id) salir iniciar)
    cargarJugadoresRestantes (cantidad-1) (lista ++ [iniciar]) (id +1) grilla salir maximo


cargarJugadoresHabilitados 0 id grilla salir maximo totaldeJugadores  = do
    cargarJugadoresRestantes (totaldeJugadores - maximo) [] maximo grilla salir maximo

cargarJugadoresHabilitados cantidad id grilla salir maximo totaldeJugadores = do

    iniciar <- Memoria.crear True

    tid <- forkIO ( Jugador.iniciar grilla (show id) salir iniciar)
    Memoria.escribir (\x -> x + 1) salir

    cargarJugadoresHabilitados (cantidad-1) (id+1) grilla salir maximo totaldeJugadores
