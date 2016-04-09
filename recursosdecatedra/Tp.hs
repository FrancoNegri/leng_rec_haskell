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

-- Punto 1
split :: Eq a => a -> [a] -> [[a]]
split del xs = filter (not . null) ( foldr ( \x (ys:rec) -> if x == del then []:(ys:rec) else (x:ys):rec ) [[]] xs ) 

-- Punto 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras =  (\frase -> mean (map (\(x,y) -> genericLength y) (cuentas (split ' ' frase)) ) )

-- Punto 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lista = nub (map (\x -> (contar (==x) lista, x)) lista)

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

-- Punto 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos =  map aplicarExtNorm textos
                where aplicarExtNorm = ( \txt -> map (\extractor -> extractor txt) extractoresNormalizados)
                      extractoresNormalizados = map (normalizarExtractor textos) extractores

-- Punto 8
distEuclideana :: Medida
distEuclideana = (\p q -> sqrt (sum (zipWith (*) (zipWith (-) p q)  (zipWith (-) p q) )))


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

-- Punto 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = ([ datos !! j | j <- [0..(n * tamPart - 1)], notElem j [((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ datos !! i | i<-[((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ etiquetas !! j | j <- [0..(n * tamPart - 1)], notElem j [((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ etiquetas !! i | i<-[((p-1) * tamPart)..(p * tamPart - 1)] ])
                                    where tamPart = div (genericLength datos) n

-- Punto 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = \xs ys -> fromIntegral (sum(zipWith f xs ys)) / fromIntegral(genericLength xs)
            where f = \x y -> if x == y then 1 else 0 

-- Punto 12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean [resultados (getParticion i) | i <- [1..n]] 
    where getParticion i = separarDatos datos etiquetas n i

resultados :: (Datos, Datos, [Etiqueta], [Etiqueta]) -> Float
resultados (entrenamiento, tests, etiquetasEnt, etiquetasTest) = accuracy (map (knn 15 entrenamiento etiquetasEnt distEuclideana) tests) etiquetasTest