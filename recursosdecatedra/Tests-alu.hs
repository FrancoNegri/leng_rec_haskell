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
		distEuclideana [1,2,3,4] [5,6,7,8] ~?= 8
	]

testDistCoseno = test [
		distCoseno [0,3,4] [0,-3,-4] ~?= -1.0,
		distCoseno [1,1,1] [1,1,1] ~?= 1,
		distCoseno [3,3,3,3,3,3] [3,3,3,3,3,3] ~?= 1
	]

----test 9
testKNN = test [
	(knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "f"
	]

--test 11
testAccuracy = test [
		accuracy ["i"] ["i"] ~?= 1,
		accuracy ["f"] ["i"] ~?= 0,
		accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6
	]