


--Ejercicio 1.5. Definir la función suma_de_cuadrados tal que suma_de_cuadrados l es la
--suma de los cuadrados de los elementos de la lista l . 
--Por ejemplo, suma_de_cuadrados [1,2,3] retorna 14.
--RECURSION

sumaCuadrados :: [Int] -> Int
sumaCuadrados [] = 0
sumaCuadrados (x:xs) = (x*x) + sumaCuadrados(xs)

--Ejercicio: VALOR ABSOLUTO de un numero con GUARDAS
valorAbs :: Int -> Int
valorAbs x | x<0 = (-x)
		   | otherwise = x

--VALOR ABSOLUTO CONDICIONALES
valorAbs2 :: Int -> Int
valorAbs2 x = if x<0 then (-x) else x

--SUMAR LOS ELEMENTOS DE UNA LISTA
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma (xs)

--DADO UN ELEMENTO Y UNA LISTA, DECIR SI EL ELEMENTO PERTENECE A LA LISTA

pertenece :: Int -> [Int] -> Bool
pertenece    n     []   = False
pertenece 	 n 	   (x:xs) | (n == x) = True
						  | otherwise = pertenece n (xs)


--DADAS DOS LISTAS, DECIR SI LA PRIMERA ES SUBLISTA DE LA SEGUNDA
--Esta funcion tiene en cuenta las permutaciones de la lista.
--Pj l1= 1,2,3  y l2= 3,2,1 retorna TRUE
subLista :: [Int] -> [Int] -> Bool
subLista 	[]	_	= True
subLista 	(x:xs) (ys) = pertenece x (ys) && subLista (xs) (ys)

--Esta funcion no tiene en cuenta las permutaciones de la lista
--Pj l1= 1,2,3  y  l2= 3,2,1 retorna FALSE
subLista2 :: [Int] -> [Int] -> Bool
subLista2		[] 		_ = True
subLista2		xs 		[]= False
subLista2		(x:xs)    (y:ys) |	(x==y) = subLista2 (xs) (ys)
							 | otherwise = subLista2 (x:xs) (ys)
 
--verifica si las listas xs e ys vistas como conjuntos, son iguales. 
--Por ejemplo,
--   igualConjunto [1..10] [10,9..1]   ==  True
--   igualConjunto [1..10] [11,10..1]  ==  False

igualConjunto:: [Int] -> [Int] -> Bool
igualConjunto [] [] = True
igualConjunto (xs) (ys) = subLista2 (xs) (ys) == subLista2 (ys) (xs)

--DEVOLVER TODOS LOS SUBCONJUNTOS DE UNA LISTA XS
subConjunto :: [Int] -> [[Int]]
subConjunto [] = [[]]
subConjunto (x:xs) = [x:ys | ys <- sub]++ sub
		where sub = subConjunto (xs)

--DEVOLVER TODOS LOS SUBCONJUNTOS DE UNA LISTA XS CON LA FUNCION MAP
subConjunto2 :: [Int] -> [[Int]]
subConjunto2 [] = [[]]
subConjunto2 (x:xs) = map (x:) sub ++ subConjunto2 (xs)
		where sub = subConjunto2 (xs)

--INTERCALA UN VALOR CON TODOS LOS ELEMENTOS DE UNA LISTA XS
intercalar :: Int -> [Int] -> [[Int]]
intercalar 	  x [] = [[x]]
intercalar 	  x (y:ys) = (x:y:ys) : [y:zs | zs <- intercalar x ys]

