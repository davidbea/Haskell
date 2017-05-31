--INTERCALAR TOMA UN ELEMENTO Y UNA LISTA, E INTERCALA ESE ELEMENTO CON CADA UNO DE LA LISTA
intercalar::a->[a]->[[a]]
intercalar n [] = [[n]]
intercalar n (x:xs) = (n:x:xs) : [x:zs|zs <- intercalar n xs]

--INTERCALAR TOMA UN ELEMENTO Y UNA LISTA, E INTERCALA EL ELEMENTO CON CADA UNO DE LOS DE LA LISTA
intercalar1::a->[a]->[[a]]
intercalar1 n [] = [[n]]
intercalar1 n (x:xs) = (n:x:xs) : [x:zs | zs <- intercalar1 n xs]

intercalar2:: a->[a]->[[a]]
intercalar2 n [] = [[n]]
intercalar2 n (x:xs)= (n:x:xs):[x:zs|zs<-intercalar2 n xs]

--INTERCALAR TOMA UNA LISTA Y UN ELEMENTO, E INTERCALA ESE ELEMENTO CON CADA UNO DE LOS DE LA LISTA
intercalar3::a->[a]->[[a]]
intercalar3 n [] = [[n]]
intercalar3 n  (x:xs) =  (n:x:xs) : [x:zs| zs <- intercalar3 n xs]

--PERMUTACIONES TOMA UNA LISTA Y DEVUELVE TODAS LAS PERMUTACIONES DE LA MISMA
permutaciones::[a]->[[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = concat [intercalar x ys| ys <- permutaciones xs]

--PERMUTACIONES TOMA UNA LISTA Y RETORNA TODAS LAS PERMUTACIONES DE LA MISMA
permutaciones2::[a]-> [[a]]
permutaciones2 [] = [[]]
permutaciones2 (x:xs) = concat [intercalar x ys|ys <- permutaciones2 xs]

--PERMUTACIONES TOMA UNA LISTA Y RETORNA LAS PERMUTACIONES DE LA MISMA
permutaciones3::[a] -> [[a]]
permutaciones3 [] = [[]]
permutaciones3 (x:xs) = concat [intercalar x ys | ys <- permutaciones3 xs]


permutaciones4::[a] -> [[a]]
permutaciones4 [] = [[]]
permutaciones4 (x:xs) = concat [intercalar x ys | ys <- permutaciones4 xs]


--ELIMINARREPE ELIMINA LOS ELEMENTOS REPETIDOS DE UNA LISTA
eliminaRepe::Eq a=>[a]->[a]
eliminaRepe [] = []
eliminaRepe (x:xs)| elem x (xs) = eliminaRepe (xs)
				  | otherwise = x : eliminaRepe (xs)


--DEVOLVER TODOS LOS SUBCONJUNTOS DE UNA LISTA
subconjuntos::[a]->[[a]]
subconjuntos [] = [[]]
subconjuntos (x:xs) = [x:zs | zs <- subconjuntos xs] ++ subconjuntos xs

--DEVOLVER TODOS LOS SUBCONJUNTOS DE UNA LISTA
subconjuntos2:: [a] -> [[a]]
subconjuntos2 [] = [[]]
subconjuntos2 (x:xs) = [x:zs | zs <- subconjuntos2 xs] ++ subconjuntos2 xs

--DEVOLVER TODOS LOS SUBCONJUNTOS DE UNA LISTA
subconjuntos3:: [a] -> [[a]]
subconjuntos3 [] = [[]]
subconjuntos3 (x:xs) = [x:zs | zs <- subconjuntos3 xs] ++ subconjuntos3 xs

--DADA UNA LISTA DEVOLVER TODAS LAS SUBSECUENCIAS DEL MISMO
subsecuencias::[a]->[[a]]
subsecuencias [] = [[]]
subsecuencias [x] = [[x]]
subsecuencias (x:xs) = (iniciales (x:xs)) ++ subsecuencias xs 

iniciales::[a]->[[a]]
iniciales [] = [[]]
iniciales (x:xs) = (x:xs) : (iniciales (init (x:xs)))

--ORDENAMIENTO POR MERGESORT
--CALCULAR LAS PRIMERAS 2 PARTES DEL ARREGLO
primera::Eq a => [a] -> [a]
primera (xs) = take (div (length xs) 2) xs

segunda::Eq a => [a] -> [a]
segunda (xs) = drop (div (length xs) 2) xs

primera2::Eq a => [a] -> [a]
primera2 (xs) = take (div (length xs) 2 ) xs

segunda2::Eq a=> [a] -> [a]
segunda2 (xs) = drop (div (length xs) 2 ) xs 

mergesort::Ord a =>[a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort (x:xs) = merge ( mergesort (primera (x:xs))) (mergesort (segunda (x:xs)))

mergesort2::Ord a => [a] -> [a]
mergesort2 [] = []
mergesort2 [x] = [x]
mergesort2 (x:xs) = merge2 (mergesort2 (primera2 (x:xs))) (mergesort2  (segunda2 (x:xs))) 

merge::Ord a => [a] -> [a] -> [a]
merge []	[] = []
merge xs	[] = xs
merge []	ys = ys	
merge (x:xs) (y:ys) | x<=y = x: merge (xs) (y:ys)
					| otherwise = y: merge (x:xs) (ys)

merge2::Ord a => [a] -> [a] -> [a]
merge2 []	[] = []
merge2  xs 	[] = xs
merge2 []	ys = ys
merge2 (x:xs) (y:ys) | x<=y = x : merge (xs) (y:ys)
					 | otherwise = y : merge (x:xs) (ys)


--INSERTAR UN ELEMENTO EN UNA LISTA ORDENADA
insertion::Ord a => a -> [a] -> [a]
insertion n [] = [n]
insertion n (x:xs) | n<x = n:x:xs
				   | otherwise = x : insertion n (xs)

--ORDENA UNA LISTA POR METODO DE ORDENACION POR INSERCION
insertionsort::Ord a => [a] -> [a]
insertionsort [] 	= []
insertionsort [x] 	= [x]
insertionsort (x:xs) = insertion x ( insertionsort xs)


--INSERTA UN ELEMENTO EN UN ARREGLO
insertion2::Ord a => a -> [a] -> [a]
insertion2 n [] = [n]
insertion2 n (x:xs) | n<=x = n:x:xs
					| otherwise = x : insertion2 n (xs)

--ALGORITMOS INSERTIONSORT QUE ORDENA

insertionsort2::Ord a => [a] -> [a]
insertionsort2 [] = []
insertionsort2 [x] = [x]
insertionsort2 (x:xs) = insertion2 x ( insertionsort2 xs)

--RETORNA EL MAXIMO DE UNA LISTA ORDENADA CON INSERTION SORT
maximoDeInsertion::Ord a => [a] -> a
maximoDeInsertion (xs) = maximo (insertionsort2 (xs))

--RETORNA EL MAXIMO DE UNA LISTA
maximo::Ord a => [a] -> a
maximo [x] = x
maximo (x:y:ys) | x>y = maximo (x:ys)
				| otherwise = maximo (y:ys) 

--METODO DE ORDENAMIENTO QUICKSORT (ORDENACION RAPIDA)
quicksort::Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
				quicksort menoresAx ++ [x] ++ quicksort mayoresAx
				where menoresAx = [y | y <- xs, y <= x]
					  mayoresAx = [y | y <- xs, y > x]
