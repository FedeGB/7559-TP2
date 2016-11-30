module Log
(
crearLog,
leer,
escribir
)where

import Control.Monad
import System.IO
import Data.Time
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Semaforo

fechaYHora = do
    currentTime <- getCurrentTime
    currentZone <- getCurrentTimeZone
    return $ (utcToLocalTime currentZone currentTime)

crearLog = atomically $ newTChan

escribir chan mensaje = do
        atomically $ writeTChan chan mensaje

leer chan archivo semaforoCerraLog = do
        mensaje <- atomically $ readTChan chan
        if ( mensaje == "SALIR" )
            then do
                hClose archivo
                Semaforo.v semaforoCerraLog
                return ()
            else do
                fecha <- fechaYHora
                let mensajeAImprimir = ( "[" ++ (show fecha) ++ "]\t" ++ mensaje )
                putStrLn mensajeAImprimir
                hPutStrLn archivo mensajeAImprimir
                leer chan archivo semaforoCerraLog