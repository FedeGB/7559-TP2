module Parser
(
obtenerAlto,
obtenerAncho,
obtenerMaximoJugadores,
obtenerCantidadDeJugadores
) where

import Data.Char
import Data.List

obtenerLineas parametros = lines parametros

altura parametro = parametro == "alto"

ancho parametro = parametro == "ancho"

maximoJugadores parametro = parametro == "maximoJugadores"

cantidadDeJugadores parametro = parametro == "cantidadDeJugadores"

obtenerParametro validador parametros = ( read ( head [ y | (x,y)<-parametros , validador x ] ) :: Int )

valorParametro parametro = drop ( ( head $ elemIndices ':' parametro ) + 1 ) parametro

tipoParametro parametro = take ( head $ elemIndices ':' parametro ) parametro

dividirLista parametros = [ ( ( tipoParametro x ) , ( valorParametro x) ) | x <- parametros ]

parser tipo listaParametros = obtenerParametro tipo ( dividirLista ( obtenerLineas listaParametros ) )

obtenerAlto listaParametros = parser altura listaParametros

obtenerAncho listaParametros = parser ancho listaParametros

obtenerMaximoJugadores listaParametros = parser maximoJugadores listaParametros

obtenerCantidadDeJugadores listaParametros = parser cantidadDeJugadores listaParametros