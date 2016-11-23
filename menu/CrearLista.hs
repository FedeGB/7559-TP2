module CrearLista
(
crearLista
) where

import Memoria

crearLista 0 lista valorInicial = do
    return lista
crearLista cantidad lista valorInicial = do
    valor <- Memoria.crear valorInicial
    crearLista (cantidad-1) (lista ++ [valor]) valorInicial