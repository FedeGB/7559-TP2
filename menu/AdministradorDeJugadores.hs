module AdministradorDeJugadores
(
cargarJugadoresHabilitados
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


cargarJugadoresRestantes 0 lista _ _ salir maximo puntuaciones= do
    forkIO (Sysadmin.mostrarPuntuaciones puntuaciones)
    esperarQueSeLibereElMaximoDeJugadores lista salir maximo

cargarJugadoresRestantes cantidad lista id grilla salir maximo puntuaciones= do
    iniciar <- Memoria.crear False
    puntos <- Memoria.crear 0
    tid <- forkIO ( Jugador.iniciar grilla (show id) salir iniciar puntos)
    cargarJugadoresRestantes (cantidad-1) (lista ++ [iniciar]) (id +1) grilla salir maximo (puntuaciones++[(id,puntos)])


cargarJugadoresHabilitados 0 id grilla salir maximo totaldeJugadores puntuaciones = do
    cargarJugadoresRestantes (totaldeJugadores - maximo) [] maximo grilla salir maximo puntuaciones

cargarJugadoresHabilitados cantidad id grilla salir maximo totaldeJugadores puntuaciones= do
    iniciar <- Memoria.crear True
    puntos <- Memoria.crear 0
    tid <- forkIO ( Jugador.iniciar grilla (show id) salir iniciar puntos)
    Memoria.escribir (\x -> x + 1) salir
    cargarJugadoresHabilitados (cantidad-1) (id+1) grilla salir maximo totaldeJugadores (puntuaciones++[(id,puntos)])
