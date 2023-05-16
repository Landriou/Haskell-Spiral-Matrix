import Data.Array

type Matriz a = Array (Int,Int) a
type Punto  = (Int, Int)
-- Funciones para manejar el calculo entre los puntos, suma y resta.
sumarPunto :: Punto -> Punto -> Punto
sumarPunto (x,y) (z,w) = (x+z, y+w)

restarPunto :: Punto -> Punto -> Punto
restarPunto (x,y) (z,w) = (x-z, y-w)

type Espiral = [Punto]
type VectorDistancia = (Int, Int) 

data PosicionRelativa = Origen|Norte|Este|Oeste|Sur|NorEste|NorOeste|SurEste|SurOeste deriving Show

-- Funcion que se encarga de determinar la posicion relativa dentro de una matriz
-- Esta posicion relativa se define como un punto cardinal dentro de los de la roseta de lso vientos
-- r son las filas ( row) y c las columnas)
posicionRelativa :: VectorDistancia -> PosicionRelativa
posicionRelativa (dr, dc) 
  | dr == 0 && dc == 0 =  Origen
  | dr == 0 && dc > 0 = Este
  | dr == 0 && dc < 0 = Oeste
  | dr > 0 && dc == 0 = Sur
  | dr < 0 && dc == 0 = Norte
  | dr > 0 && dc > 0 = SurEste
  | dr > 0 && dc < 0 = SurOeste
  | dr < 0 && dc > 0 = NorEste
  | dr < 0 && dc < 0 = NorOeste

-- Direcciones posibles en una matriz.
arriba    = (-1, 0)
abajo  = (1,0)
izquierda  = (0, -1)
derecha = (0,1)

-- Función utilizada para decidir el siguiente punto en la matriz y agregarlo a la lista
-- Funciona como una rosa de los vientos.
proxPunto :: PosicionRelativa -> VectorDistancia -> VectorDistancia
proxPunto Origen _ = izquierda
proxPunto Norte _      = izquierda
proxPunto Oeste _      = abajo
proxPunto Sur _      = derecha
proxPunto Este _      = arriba
proxPunto NorOeste (filas, columnas) | abs filas > abs columnas = izquierda
                   | abs columnas >= abs filas = abajo
proxPunto SurOeste (filas, columnas) | abs filas >= abs columnas = derecha
                   | abs columnas > abs filas = abajo
proxPunto NorEste (filas, columnas) | abs filas > abs columnas = arriba
                   | abs columnas >= abs filas = izquierda
proxPunto SurEste (filas, columnas) | abs filas > abs columnas = derecha
                   | abs columnas >= abs filas = arriba

-- Calcula el siguiente punto a moverse y llamando a las funciones anteriores para determinarlo. 
siguiente :: Punto -> Punto -> Punto
siguiente origen punto = sumarPunto punto (proxPunto posRelativa vecDist)
  where  
    posRelativa  = posicionRelativa vecDist
    vecDist = restarPunto punto origen

-- Funcion recursiva que llama a la funcion 'siguiente' para ir generando los puntos e ir decidiendo las posiciones en base a las posiciones relativas cardinales.
generarEspiral :: Punto -> Punto -> Espiral 
generarEspiral origen punto =  punto:generarEspiral origen (siguiente origen punto)

-- Esta funcion es el trigger de todo el proceso para generar el espiral, utiliza las HIGH ORDER FUNCTIONS map y filter para poder generar la matriz de la forma mas general que pude encontrar, luego llama a generar Espiral desde un punto a otro.,
espiral' :: Int -> Int -> Int -> Int -> [Int]
espiral' filas columnas x y = map (\(a,b) -> (a-1)*columnas + b) $ 
                      take (filas*columnas) $ 
                        filter (\(x,y) -> x>=1 && x<=filas && y>=1 && y<=columnas) $
                          generarEspiral origen origen
  where 
    origen = (x,y)

-- Funcion Wrapper, como el proceso termina haciendo una espiral que va desde adentro hacia fuera, se pone en reversa la lista resultante.
espiral :: Int -> Int -> [Int]
espiral filas columnas = reverse(espiral' filas columnas 2 2)

main :: IO ()
main =  do
        print(espiral 3 3)