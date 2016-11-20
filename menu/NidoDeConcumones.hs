module NidoDeConcumones
(
iniciarNido
) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Grilla
import Memoria
import MovimientosUtils


inicarNido maxConcumones tMovimiento grilla = do
