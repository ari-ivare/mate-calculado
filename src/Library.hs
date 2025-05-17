module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Postre = UnPostre {
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

type Hechizo = Postre -> Postre

torta = UnPostre ["crema", "ddl"] 100 5

incrementoTemp = 1
perdidaPeso = 0.05
incendio :: Hechizo
incendio postre = postre{temperatura = temperatura postre + incrementoTemp, peso = peso postre - peso postre * perdidaPeso }

immobulus :: Hechizo
immobulus postre = postre{temperatura = 0}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre{sabores = sabor:sabores postre}

perdidaPesoWL = 0.1
wingardiumLeviosa :: String -> Hechizo
wingardiumLeviosa sabor postre = (agregarSabor sabor postre){peso = peso postre - peso postre * perdidaPesoWL}

deffindo :: Number -> Hechizo
deffindo porcentaje postre = postre{peso = peso postre - peso postre * porcentaje / 100}

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedavra :: Hechizo
avadaKedavra postre = (immobulus postre){sabores = []}

estaCongelado :: Postre -> Bool
estaCongelado = (0>=).temperatura

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor = not.null.sabores

tienePeso :: Postre -> Bool
tienePeso = (0<=).peso

estaListo :: Postre -> Bool
estaListo postre = (not.estaCongelado) postre && tieneAlgunSabor postre && tienePeso postre

postres = [torta, immobulus torta, incendio torta]

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (estaListo.hechizo)

pesoPromedioPostresListos :: [Postre] -> Number
pesoPromedioPostresListos postres = sum (map peso (filter estaListo postres))

data Mago = UnMago {
    hechizos :: [Hechizo],
    cantHorrocruxes :: Number
} deriving (Show, Eq)

tortaVacia = UnPostre [] 100 50

magoEntrenado :: Mago -> Hechizo -> Postre -> Mago
magoEntrenado mago hechizo postre | hechizo postre == avadaKedavra postre = (agregarHorrocruxes.agregarHechizo hechizo) mago
                                  | otherwise = agregarHechizo hechizo mago

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago{hechizos = hechizo:hechizos mago}

agregarHorrocruxes :: Mago -> Mago
agregarHorrocruxes mago = mago{cantHorrocruxes = cantHorrocruxes mago + 1 }


cantidadSabores :: Postre -> Number
cantidadSabores = length.sabores

cantSaboresPostHechizo :: Hechizo -> Postre -> Number
cantSaboresPostHechizo hechizo = cantidadSabores.hechizo

mejorHechizo :: Postre -> Hechizo -> Hechizo -> Hechizo
mejorHechizo postre hechizoA hechizoB | cantSaboresPostHechizo hechizoA postre > cantSaboresPostHechizo hechizoB postre = hechizoA
                                      | otherwise = hechizoB

listaHechizos = [immobulus, avadaKedavra, riddikulus "chocominto"]

elMejorHechizo :: [Hechizo] -> Postre -> Hechizo
elMejorHechizo hechizos postre = foldl1 (mejorHechizo postre) hechizos

listaPostresInf = repeat torta
listaHechizosInf = repeat immobulus

{-
Magia Infinita

3.B
No, no existe una consulta que me sepa dar una respuesta.
La funcion para saber si un hechizo deja listos a todos los postres de una mesa usa la función all.
Esta depende de que absolutamente todos los valores de la lista cumplan con determinada condición por lo que no es posible una lazy evaluation que permita cortar antes la evaluación de la lista.

3.C
No, no existe una consulta que me sepa dar una respuesta.
El mejor hechizo se calcula por medio de un foldl1 y se van comparando uno a uno los hechizos de toda la lista, por lo que debe evaluarse la lista en su totalidad para poder devolver un resultado.
Si la condición para evaluar el mejor hechizo fuese distinta podría llegar a darse el caso.
Por ejemplo si se considera como mejor hechizo "al primero en encontrarse que deje a un postre con 10 sabores".
En ese caso podría cortase pronto la evaluación y obtener un resultado. Pero no es el caso.

-}