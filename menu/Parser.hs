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

obtenerParametro validador parametros = ( read ( head [ y | (x,y)<-parametros , validador x ] ) :: Int )

valorParametro parametro = drop ( ( head $ elemIndices ':' parametro ) + 1 ) parametro

tipoParametro parametro = take ( head $ elemIndices ':' parametro ) parametro

dividirLista parametros = [ ( ( tipoParametro x ) , ( valorParametro x) ) | x <- parametros ]

parser listaParametros = dividirLista ( obtenerLineas listaParametros )

obtenerAlto listaParametros = obtenerParametro altura ( parser listaParametros )

obtenerAncho listaParametros = obtenerParametro ancho ( parser listaParametros )

obtenerMaximoJugadores listaParametros = obtenerParametro maximoJugadores ( parser listaParametros )