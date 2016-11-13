module Memoria
(
crear,
leer,
escribir
) where
import System.IO
import Control.Concurrent
import Control.Concurrent.STM

crear valorInicial = atomically (newTVar valorInicial)

leer = readTVar

{-f : funcion que se le aplica al dato-}
escribir f valor = do
    atomically ( do dato <- readTVar valor
                    writeTVar valor (f dato) )