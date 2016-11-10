module MemoriaCompartida
(
crear,
crear2,
leer,
escribir
) where
import System.IO
import Control.Concurrent
import Data.IORef
--import Control.Concurrent.STM

crear valorInicial = atomically (newTVar valorInicial)
crear2 valorInicial = newTVar valorInicial

leer = atomically . readTVar

{-f : funcion que se le aplica al dato-}
escribir f valor = do
	atomically ( do dato <- readTVar valor
	                writeTVar valor (f dato) )