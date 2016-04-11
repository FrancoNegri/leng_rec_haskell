module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = if null xs then 0 else realToFrac (sum xs) / genericLength xs
-- Agregamos el chequeo por null para evitar tener que chequearlo en todas las funciones que utilizan a mean.


-- Punto 1
split :: Eq a => a -> [a] -> [[a]]
split del xs = filter (not . null) ( foldr ( \x (ys:rec) -> if x == del then []:(ys:rec) else (x:ys):rec ) [[]] xs ) 

-- Punto 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras =  (\frase -> mean (map (\(x,y) -> genericLength y) (cuentas (split ' ' frase)) ) )

-- Punto 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lista = nub (map (\x -> (contar (==x) lista, x)) lista)
-- Usamos nub para eliminar los repetidos, ya que si un elemento aparece n veces entonces vamos a obtener n veces la tupla "(n, elemento)"

contar :: Eq a => (a  -> Bool) -> [a] -> Int
contar a = genericLength . filter a

-- Punto 4
repeticionesPromedio :: Extractor
repeticionesPromedio = (\texto -> mean ( (map (fromIntegral . fst) (cuentas (split ' ' texto)) ) ) )

-- Punto 5
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ \texto -> fromIntegral(contar (==tok) texto) / genericLength texto | tok <- tokens ]

-- Punto 6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor txts extractor = (\texto -> (extractor  texto) / norma) where norma = maximum (map (abs . extractor) txts)
-- Dividimos el resultado del extractor por el máximo obtenido de aplicarlo a todos los textos.

-- Punto 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos =  map aplicarExtNorm textos
                where aplicarExtNorm = ( \txt -> map (\extractor -> extractor txt) extractoresNormalizados)
                      extractoresNormalizados = map (normalizarExtractor textos) extractores

-- Punto 8
distEuclideana :: Medida
distEuclideana = (\p q -> sqrt (sum (zipWith (*) (producto p q)  (producto p q) )))
                  where producto a b = zipWith (-) a b


distCoseno :: Medida
distCoseno = (\p q -> (prodVect p q) / (norma p * norma q))
                  where prodVect p q = sum(zipWith (*) p q)
                        norma r = sqrt(prodVect r r)

-- Punto 9
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas distancia origen = moda (map snd kVecinosMasCercanos)
    where kVecinosMasCercanos = take k (sort distanciasYEtiquetas)
          distanciasYEtiquetas = zip distancias etiquetas
          distancias = map (distancia origen) datos

moda :: [Etiqueta] -> Etiqueta
moda etiquetas = snd (maximum (cuentas etiquetas))
-- La funcion "cuentas" arma una tupla (#apariciones, etiqueta).
-- Como la tupla pertenece a la clase Ord, se puede sacar el maximo sin usar maximumBy. 
-- Usamos "snd" para quedarnos solo con la etiqueta.

-- Punto 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = ([ datos !! j | j <- [0..(n * tamPart - 1)], notElem j [((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ datos !! i | i<-[((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ etiquetas !! j | j <- [0..(n * tamPart - 1)], notElem j [((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ etiquetas !! i | i<-[((p-1) * tamPart)..(p * tamPart - 1)] ])
                                    where tamPart = div (genericLength datos) n
-- La primer y la tercera lista toman todos los elementos de datos/etiquetas excepto los que corresponden
-- a la partición de entrenamiento (la p-esima partición) y los que sobran al final por dividir en particiones de tamaño n.
-- La segunda y cuarta listas toman los elementos correspondientes a la partición p-esima, es decir, la partición de entrenamiento.

-- Punto 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = \xs ys -> fromIntegral (sum(zipWith iguales xs ys)) / fromIntegral(genericLength xs)
           where iguales = \x y -> if x == y then 1 else 0

-- Punto 12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean [resultados (getParticion i) | i <- [1..n]] 
    where getParticion i = separarDatos datos etiquetas n i

resultados :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> Float
resultados (entrenamiento, tests, etiquetasEnt, etiquetasTest) = accuracy (map (knn 15 entrenamiento etiquetasEnt distEuclideana) tests) etiquetasTest