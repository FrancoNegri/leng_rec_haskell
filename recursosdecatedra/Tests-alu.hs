-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 	"split" ~: testsSplit,
 	"cuentas" ~: testsCuentas,
	"repeticionesPromedio" ~: testRepeticionesPromedio,
	"extraerFeatures" ~: testExtraerFeatures
 	]

testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "],
	split ',' " ,, " ~?= [" "," "],
	split ',' "" ~?= [],
	split ',' "," ~?= [],
	split ',' "a,PLP,a" ~?= ["a","PLP","a"]
  	]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")]
	]

testRepeticionesPromedio = test [
	repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5,
	repeticionesPromedio "" ~?= 0,
	repeticionesPromedio " " ~?= 0,
	repeticionesPromedio "asa, casado, asado" ~?= 1,
	repeticionesPromedio "lalala lalala lalala" ~?= 3
	]

text1 = "n = succ ( succ ( succ 2 ) )"
text2 = "a++; a = 2; b = a;"
text3 = "while ( true ) a++ ;"
textos = ["", " ", text1, text2, text3]

testExtraerFeatures = test [
	extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] textos ~?= 
	[[0,0], [0,0], [0.6,1], [0.73333335,0.70], [1,0.60]] --normalizado
	--[[0,0],[0,0],[9/6 = 1.5, 10/6 = 1.66], [11/6 = 1.83, 1], [15/6 = 2.5, 1]] (sin normalizar)
	]
	