cuentas [] = []
cuentas (x:xs)
	| estaEnLista x xs = sumar x (cuentas xs)
	| otherwise = [(1,x)]++cuentas xs--si no esta

sumar key [] = []
sumar key (x:xs )
	| key == (snd x) = [(1 + (fst x),snd x )] ++ (sumar key xs)
	| otherwise = [x] ++ (sumar key xs)

estaEnLista elem [] = False
estaEnLista elem (x:xs) = (elem == x)||(estaEnLista elem xs)