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
mean xs = realToFrac (sum xs) / genericLength xs

-- Punto 1
split :: Eq a => a -> [a] -> [[a]]
split delimiter = foldr f [[]] 
          where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

-- Punto 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras frase =  mean (map genericLength (split ' ' frase))

-- Punto 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lista = nub (map (\x -> (contar (==x) lista, x)) lista)

contar a = length . filter a

-- Punto 4
repeticionesPromedio :: Extractor
repeticionesPromedio = undefined

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = undefined

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = undefined

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = undefined

distEuclideana :: Medida
distEuclideana = undefined

distCoseno :: Medida
distCoseno = undefined

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined