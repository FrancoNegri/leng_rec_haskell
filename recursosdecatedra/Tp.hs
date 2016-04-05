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
split del xs = sacarVacios ( foldr ( \x (ys:rec) -> if x == del then []:(ys:rec) else (x:ys):rec ) [[]] xs ) 

sacarVacios :: Eq a => [[a]] -> [[a]]
sacarVacios = foldr ( \xs rec -> if xs == [] then rec else (xs:rec) ) []

-- Punto 2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras frase =  mean (map genericLength (split ' ' frase))

-- Punto 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lista = nub (map (\x -> (contar (==x) lista, x)) lista)

contar :: Eq a => (a  -> Bool) -> [a] -> Int
contar a = genericLength . filter a

-- Punto 4
repeticionesPromedio :: Extractor
repeticionesPromedio texto = mean ( (map (\(x, y) -> fromIntegral x) (cuentas (split ' ' texto)) ) )

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
extraerFeatures exs txts = [ [extractor  txt | extractor <- [normalizarExtractor txts ex | ex <- exs ]] | txt <- txts ]

-- Punto 8
distEuclideana :: Medida
distEuclideana p q = sqrt (sum (map (\x -> x*x) (zipWith (-) p q)))

distCoseno :: Medida
distCoseno p q = (prodVect p q) / (norma p * norma q)
                  where prodVect p q = sum(zipWith (*) p q)
                        norma r = sqrt(prodVect r r)
-- Punto 9
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medida =  (\nuevoDato -> snd (maximo (cuentas (getk k (ordenar (zipWith (\x y -> (x,y)) (map (medida nuevoDato) datos) etiquetas))))))

orden x z | fst x < fst z = LT
          |otherwise = GT
ordenar xs = sortBy orden xs
 
getk k lista = map (\x -> snd x) (take k lista)

ordenEtiquetas x z | fst x < fst z = LT
                   |otherwise = GT

maximo = maximumBy ordenEtiquetas 

-- Punto 9 (Solución de Javier)
--knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
--knn cantVecinos datos etiquetas medida instancia = obtenerModa ( vecinosMasCercanos cantVecinos ( etiquetar (obtenerMedidas instancia medida datos) etiquetas)  )

--obtenerMedidas :: Instancia ->Medida ->Datos ->[(Float, Instancia)]
--obtenerMedidas instanciaOrigen medida datos = [ (medida instanciaOrigen instancia, instancia) | instancia <- datos ]

--etiquetar :: [(Float, Instancia)] -> [Etiqueta] -> [(Float, Etiqueta)]
--etiquetar medidas etiquetas = [ ( fst ( medidas !! i ), etiquetas !! i ) | i <- [0..(genericLength etiquetas -1)] ]

--vecinosMasCercanos :: Int ->[(Float, Etiqueta)] -> [Etiqueta]
--vecinosMasCercanos n medidas = map (\x -> snd x) (take n ( sortBy (compararDatos) medidas ))

--compararDatos :: (Float, Etiqueta) ->(Float, Etiqueta) ->Ordering
--compararDatos (x1,x2) (y1,y2) = if x1 < y1 then LT else GT

--obtenerModa :: [Etiqueta] ->Etiqueta
--obtenerModa etiquetas = snd ( maximumBy (compararFrecuencias) (cuentas etiquetas) )

--compararFrecuencias :: (Int, Etiqueta) ->(Int, Etiqueta) ->Ordering
--compararFrecuencias (x1,x2) (y1,y2) = if x1 < y1 then LT else GT



-- Punto 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = ([ datos !! j | j <- [0..(n * tamPart - 1)], notElem j [((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ datos !! i | i<-[((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ etiquetas !! j | j <- [0..(n * tamPart - 1)], notElem j [((p-1) * tamPart)..(p * tamPart - 1)] ],
                                    [ etiquetas !! i | i<-[((p-1) * tamPart)..(p * tamPart - 1)] ])
                                    where tamPart = div (genericLength datos) n

-- Punto 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = \xs ys -> sum(zipWith f xs ys) / fromIntegral(genericLength xs)
            where f = \x y -> if x == y then 1 else 0 

-- Punto 12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean [fold n datos etiquetas i  | i <- [0..14]] 

fold n datos etiquetas i = accuracy (map (knn 15 entrenamiento etiquetasEnt distEuclideana) test) etiquetasTest
  where particion = separarDatos datos etiquetas n i
  	entrenamiento = primero particion
  	test = segundo particion
  	etiquetasEnt = tercero particion
 	etiquetasTest = cuarto particion

primero (a,_,_,_) = a
segundo (_,a,_,_) = a
tercero (_,_,a,_) = a
cuarto (_,_,_,a) = a