-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
	"split" ~: testsSplit,
 	"longitud" ~: testlongitud,
 	"cuentas" ~: testsCuentas,
	"repeticionesPromedio" ~: testRepeticionesPromedio,
	"frecuenciasTokens" ~: testFrecuenciasTokens,
	"normalizarExtractor" ~: testNormalizarExtractor,
	"extraerFeatures" ~: testExtraerFeatures,
	"distEuclideana" ~: testDistEuclideana,
	"distCoseno" ~: testDistCoseno,
	"KNN" ~: testKNN,
	"accuracy" ~: testAccuracy
 	] 

------test 1
testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "],
	split ',' " ,, " ~?= [" "," "],
	split ',' "" ~?= [],
	split ',' "," ~?= [],
	split ',' "a,PLP,a" ~?= ["a","PLP","a"]
  	]

-----test 2
testlongitud = test [
	longitudPromedioPalabras "hola" ~?= 4,
	longitudPromedioPalabras "hola holas" ~?= 4.5,
	longitudPromedioPalabras "a a a a a" ~?= 1,
	longitudPromedioPalabras " " ~?= 0,
	longitudPromedioPalabras "          " ~?= 0
	]

-----test 3
testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
	--cuentas [] ~?= [], // Da eso, pero tira un error raro HUnit
	cuentas ["a"] ~?= [(1, "a")],
	cuentas ["f++", "i++", "++", "i++", "j++", "++", "i++"] ~?= [(1, "f++"), (3, "i++"), (2, "++"), (1, "j++")]
	]

------test 4
testRepeticionesPromedio = test [
	repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5,
	repeticionesPromedio "" ~?= 0,
	repeticionesPromedio " " ~?= 0,
	repeticionesPromedio "asa, casado, asado" ~?= 1,
	repeticionesPromedio "lalala lalala lalala" ~?= 3
	]

------test 5
testFrecuenciasTokens = test [
	(head frecuenciaTokens) "_o_l_a" ~?=0.5,
	(head frecuenciaTokens) "hola" ~?=0,
	(head (tail frecuenciaTokens)) ",,," ~?=1
	]

-----test 6
-- La lista vaciá: [], no cumple la precondición
testNormalizarExtractor = test [
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] longitudPromedioPalabras) "a a a a a" ~?= 0.125,
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] longitudPromedioPalabras) "hola hola" ~?= 0.50,
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] longitudPromedioPalabras) " " ~?= 0.0,
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] longitudPromedioPalabras) "while(true){ i++}" ~?= 1.0,
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] repeticionesPromedio) "a a a a a" ~?= 1.0,
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] repeticionesPromedio) "hola hola" ~?= 0.4,
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] repeticionesPromedio) " " ~?= 0.0,
	(normalizarExtractor ["a a a a a", "hola hola", " ", "while(true){ i++}"] repeticionesPromedio) "while(true){ i++}" ~?= 0.2
	]
	
-----test 7
text1 = "n = succ ( succ ( succ 2 ) )"
text2 = "a++; a = 2; b = a;"
text3 = "while ( true ) a++ ;"
textos = ["", " ", text1, text2, text3]

testExtraerFeatures = test [
	extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] textos ~?= 
	[[0,0], [0,0], [0.6,1], [0.73333335,0.70], [1,0.60]] --normalizado
	--[[0,0],[0,0],[9/6 = 1.5, 10/6 = 1.66], [11/6 = 1.83, 1], [15/6 = 2.5, 1]] (sin normalizar)
	]
	
----test 8
testDistEuclideana = test [
		distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464,
		distEuclideana [1,1,1] [1,1,1] ~?= 0,
		distEuclideana [1,2,3,4] [5,6,7,8] ~?= 8,
        distEuclideana [5,6,7,8] [1,2,3,4] ~?= 8
	]

testDistCoseno = test [
		distCoseno [0,3,4] [0,-3,-4] ~?= -1.0,
        distCoseno [0,-3,-4] [0,3,4] ~?= -1.0,
		distCoseno [1,1,1] [1,1,1] ~?= 1,
		distCoseno [3,3,3,3,3,3] [3,3,3,3,3,3] ~?= 1
	]

----test 9
testKNN = test [
	(knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~=? "f",
	(knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","i","i","i"] distEuclideana) [1,1] ~=? "i",
	(knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["f","f","f","f","f"] distEuclideana) [1,1] ~=? "f",
	(knn 2 [[0,1],[0,2],[0,3],[0,4],[0,5]] ["f","f","i","i","i"] distEuclideana) [0,0] ~=? "f",
	(knn 2 [[0,1],[0,2],[0,3],[0,4],[0,5]] ["i","i","f","f","f"] distEuclideana) [0,0] ~=? "i"
	]

----test 10
datos10 = [[0,0],[0,0],[1.5, 1.66], [1.71, 1.16], [2.5, 1]]
etiquetas10 = ["f", "i", "f", "f", "i"]
testSepararDatos = test [
	separarDatos datos10 etiquetas10 1 1 ~?= ( [], datos10, [], etiquetas10),
	separarDatos [] [] 1 1 ~?= ( [], [], [], []),
	separarDatos [] [] 5 1 ~?= ( [], [], [], []),
	separarDatos [] [] 5 4 ~?= ( [], [], [], []),
	separarDatos [[1]] ["f"] 1 1 ~?= ( [], [[1]], [], ["f"] ),
	separarDatos datos10 etiquetas10 5 2 ~?= ( [[0,0],[1.5, 1.66], [1.71, 1.16], [2.5, 1]], [[0,0]], ["f", "f", "f", "i"], ["i"]),
	separarDatos datos10 etiquetas10 2 2 ~?= ( [[0,0],[0,0]], [[1.5, 1.66], [1.71, 1.16]], ["f", "i"], ["f", "f"])
	]

--test 11
testAccuracy = test [
		accuracy ["i"] ["i"] ~?= 1,
		accuracy ["f"] ["i"] ~?= 0,
		accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6
	]
    
--test 12
datos12 = [[0,1],[0,2],[2,1],[1,1],[2,3],[0,2],[2,1],[1,1],[2,3]]
etiquetas12 = ["i","i","f","f","i","i","f","f","i"]

-- Las tres particiones para calcular accuracy
-- train_i = datos de entrenamiento de la particion i
-- val_i = datos de validacion de la particion i

(train_1, val_1, etTrain_1, etVal_1) = separarDatos datos12 etiquetas12 3  1
(train_2, val_2, etTrain_2, etVal_2) = separarDatos datos12 etiquetas12 3  2
(train_3, val_3, etTrain_3, etVal_3) = separarDatos datos12 etiquetas12 3  3

-- etiquetasClasificada_i_j = etiqueta del j-esimo punto de val_i a todos los de train_i
etiquetasClasificada_1_1 = knn 15 train_1 etTrain_1 distEuclideana (val_1 !! 0)
etiquetasClasificada_1_2 = knn 15 train_1 etTrain_1 distEuclideana (val_1 !! 1)
etiquetasClasificada_1_3 = knn 15 train_1 etTrain_1 distEuclideana (val_1 !! 2)

etiquetasClasificada_2_1 = knn 15 train_2 etTrain_2 distEuclideana (val_2 !! 0)
etiquetasClasificada_2_2 = knn 15 train_2 etTrain_2 distEuclideana (val_2 !! 1)
etiquetasClasificada_2_3 = knn 15 train_2 etTrain_2 distEuclideana (val_2 !! 2)

etiquetasClasificada_3_1 = knn 15 train_3 etTrain_3 distEuclideana (val_3 !! 0)
etiquetasClasificada_3_2 = knn 15 train_3 etTrain_3 distEuclideana (val_3 !! 1)
etiquetasClasificada_3_3 = knn 15 train_3 etTrain_3 distEuclideana (val_3 !! 2)

etiquetasClasificadas1 = [ etiquetasClasificada_1_1, etiquetasClasificada_1_2, etiquetasClasificada_1_3 ] -- = ["i","i","i"]
etiquetasClasificadas2 = [ etiquetasClasificada_2_1, etiquetasClasificada_2_2, etiquetasClasificada_2_3 ] -- = ["i","i","i"]
etiquetasClasificadas3 = [ etiquetasClasificada_3_1, etiquetasClasificada_3_2, etiquetasClasificada_3_3 ] -- = ["i","i","i"]

--accuracy etiquetasClasificadas1 = 1
--accuracy etiquetasClasificadas2 = 1
--accuracy etiquetasClasificadas3 = 1

testNFoldCrossValidation = test [
		nFoldCrossValidation 3 datos12 etiquetas12 ~?= 1,
        nFoldCrossValidation 3 [] [] ~?= 1,
        nFoldCrossValidation 1 datos12 etiquetas12 ~?= 1
    ] 