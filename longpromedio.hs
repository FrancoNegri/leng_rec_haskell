import Data.List

split del [] = [[]]
split del (x:xs)
		| x == del = []:listaDeListas
		--Si x es igual al delimitador, concateno una nueva lista a la solución donde voy a ir agregando la palabra
		| otherwise = [[x]++head listaDeListas]++tail listaDeListas
		--Si x no es el delimitador, lo agrego a la lista que esta en la cabeza, concateno la nueva cabeza con la cola y devuelvo el resultado
		where listaDeListas = split del xs
		--solo por claridad y para no llenar de parentesis pongo el where

--mean hecho por la catedra, ver recursoscatedra/Tp.hs
mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

--No hace nada raro, usa las dos funciones que dice el tp que uses
longitudPromedioPalabras texto = mean (map genericLength (split ' ' texto))