module Parser
(
obtenerAlto,
obtenerAncho,
obtenerMaximoJugadores
) where

import Data.Char
import Data.List

obtenerLineas parametros = lines parametros

altura parametro = parametro == "alto"

ancho parametro = parametro == "ancho"

maximoJugadores parametro = parametro == "maximoJugadores"

obtenerParametro validador parametros = digitToInt ( head ( head [ y | (x,y)<-parametros , validador x ] ) )

valorParametro parametro = drop ( ( head $ elemIndices ':' parametro ) + 1 ) parametro

tipoParametro parametro = take ( head $ elemIndices ':' parametro ) parametro

dividirLista parametros = [ ( ( tipoParametro x ) , ( valorParametro x) ) | x <- parametros ]

obtenerAlto listaParametros = obtenerParametro altura ( dividirLista ( obtenerLineas listaParametros ) )

obtenerAncho listaParametros = obtenerParametro ancho ( dividirLista ( obtenerLineas listaParametros ) )

obtenerMaximoJugadores listaParametros = obtenerParametro maximoJugadores ( dividirLista ( obtenerLineas listaParametros ) )