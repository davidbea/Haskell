	 --suma
suma:: Int -> Int -> Int
suma x y = x+y

--resta
resta:: Int -> Int -> Int
resta x y = x-y

--division
divide:: Float -> Float -> Float
divide x y = x/y

--exponente
exp:: Int -> Int -> Int
exp x 0 = 1
exp x y = x^y

--igualdad
igu:: Int -> Int -> Bool
igu x y = x==y

--distinto
dist:: Int -> Int -> Bool
dist x y = x/=y

--menor
men:: Int -> Int -> Bool
men x y = x<y

--mayor
may:: Int -> Int -> Bool
may x y = x>y

--menorigual
menigu:: Int -> Int -> Bool
menigu x y = x<=y

--mayorigual
mayigu:: Int -> Int -> Bool
mayigu x y = x>=y

--conjuncion
conj :: Bool -> Bool -> Bool
conj x y = x && y

--disyuncion
disy :: Bool -> Bool -> Bool
disy x y = x || y

--lista añadiendo x al principio de la lista
add:: a -> [a] -> [a]
add x ys = x:ys

--concatenacion
concat1:: [a] -> [a] -> [a]
concat1 [] [] = []
concat1 [] (y:ys) = (y:ys)
concat1 (x:xs) []= (x:xs)
concat1 (x:xs) (y:ys)= (x:xs) ++ (y:ys)

--devuelve el n-esimo elemento de la lista
nesimo:: Int -> [Int] -> Int
nesimo 0 (x:xs)= x
nesimo n []=0
nesimo n (x:xs)= nesimo (n-1) (xs)

--aplica la funcion en todos los elementos
map1:: (a->b)-> [a]-> [b]
map1 f [] = []
map1 f (x:xs)= (f x) : (map f xs)

--valor absoluto de x
abs1:: Int -> Int
abs1 x | x>0 = x
       | x<0 = (-x)

--devuelve la conjuncion entre dos listas
conjbool :: [Bool]->[Bool]->[Bool]
conjbool [][]=[]
conjbool (x:xs)(y:ys)= (x&&y) : conjbool xs ys

--devuelve el elemento siguiente al ingresado
ceiling:: Int -> Int
ceiling x= x+1

--tira los n primeros elementos

drop1:: Int->[a]->[a]
drop1 n []=[]
drop1 0 (x:xs)=(x:xs)
drop1 n (x:xs)=drop1 (n-1) (xs)

--devuelve si pertenece un elemento a una lista dada
perte:: Int -> [Int] -> Bool
perte n [] = False
perte n (x:xs) = (n==x) || perte n xs

--filtra los elementos que cumple con el predicado p
filter1:: (a->Bool)->[a]->[a]
filter1 (p) []= []
filter1 (p) (x:xs)= if p x then x: filter1 (p) (xs)
					else filter1 (p) (xs) 

--pliega a la derecha
foldrm :: (a -> b -> b) -> b -> [a] -> b
foldrm f z [] = z
foldrm f z (x:xs) = f x (foldrm f z xs)

--pliega a la izquierda
foldlm :: (a -> b -> b) -> b -> [a] -> b
foldlm f z [] = z
foldlm f z (x:xs) = foldlm f (f x z) xs

--retorna el ultimo elemento de una lista
lastm:: [a]->a
lastm [x]=x
lastm (x:xs)=lastm xs

--retorna el ultimo elemento de una lista
initm:: [Int]->[Int]
initm [x]=[]
initm (x:xs)=x : (initm xs)

--reversa
revers:: [Int]->[Int]
revers []=[]
revers (x:xs)= (last (x:xs)) : revers(initm (x:xs))

--zipea los elementos de ambas listas y lo pone en pares
zipm:: [a]->[b]->[(a,b)]
zipm [][]=[]
zipm (x:xs)(y:ys)=(x,y):zipm xs ys

--derivacion de programa, calcula la suma de las primeras potencias
function:: Int-> Int-> Int
function x 0 = 1
function x (n+1) = x^(n+1) + function x n 

--devuelve la lista de los primos menores a n
primos:: Int-> [Int]
primos n = [x | x <-[2..n], primo x x]

--devuelve si es primo (bool)
primo :: Int -> Int-> Bool
primo n 1= True
primo n 2= True
primo n x= ((mod n (x-1)) /= 0) && (primo n (x-1))  

--anagrama dice si una lista puede obtenerse a partir de la otra
anagrama :: [Int]->[Int]-> Bool
anagrama [][]=True
anagrama [] (y:ys)=True
anagrama (x:xs)[]= False
anagrama (x:xs)(y:ys)=(pertenece x (y:ys)) && (anagrama xs (y:ys))

anagrama2 :: [Int]->[Int]-> Bool
anagrama2 [][]=True
anagrama2 [] (y:ys)=True
anagrama2 (x:xs)[]= False
anagrama2 (x:xs) (y:ys) | pertenece x (y:ys) && pertenece y (x:xs) = anagrama2 (xs) (ys)
					   | otherwise = False



--pertenece me dice si un elemento esta en una cadena
pertenece:: Int-> [Int] -> Bool
pertenece x [] = False
pertenece x (y:ys) = (x==y) || (pertenece x ys)

--Inserta un elemento en la posicion dada-- insertar:: elemento a insertar -> posicion-> lista en la que inserta -> lista con elemento
insertar:: Int-> Int->[Int]->[Int]
insertar e p [] = [e]
insertar e p (x:xs)| p==0 = e:(x:xs)
		   | p/=0 = x: insertar e (p-1) (xs) 

--te devuelve todas las permutaciones de una lista  
permutaciones:: [a]->[[a]]
permutaciones []= [[]]		
permutaciones (x:xs)= concat [intercala x ys| ys <- permutaciones xs]

-- toma un elemento con una lista y devuelve lista de listas con el elemento intercalado en todas las posiciones
intercala:: a->[a]->[[a]]
intercala n [] = [[n]]
intercala n (x:xs)= (n:x:xs):[x:zs|zs<-intercala n xs]

--toma una lista, genera las permutaciones y despues devuelvo la lista ordenada
slowsort:: Ord a => [a] -> [a]
slowsort [x]= [x]
slowsort []= []
slowsort xs= head [a | a <- permutaciones (xs), ord (a)]

--te dice si una lista esta ordenada
ord:: Ord a =>[a] -> Bool
ord [x] = True
ord [] = True
ord (x:y:xs)= (x<=y) && ord(y:xs)

inversa1:: [a]->[a]
inversa1 []=[]
inversa1 (x:xs)=(inversa1 xs)++[x]

--funcion que devuelve los enteros pares positivos
positivos:: [Int] -> [Int]
positivos []=[]
positivos (x:xs)|(mod x 2 == 0) = [x]++positivos xs
			    |(mod x 2 /= 0) = positivos xs
			    
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):(map (\s -> y:s) (interleave x ys))

permutaciones1:: [Int] -> [[Int]]
permutaciones1 []  = [[]]
permutaciones1 [x] = [[x]]
permutaciones1 (x:xs) = concat (map (\s -> (interleave x s)) (permutaciones1 xs))

--devuelve la lista de todas las listas unitarias
listaunit :: [Int]->[[Int]]
listaunit []=[]
listaunit (x:xs) = [x]:listaunit xs 

--devuelve subconjuntos de una lista
subconjuntos :: [a]-> [[a]]
subconjuntos []= [[]]
subconjuntos (x:xs)= sub ++ map(x:) sub
	where sub = subconjuntos xs		

--Ejercicio 3 que ordena una lista de elementos (por ejemplo, de enteros) mediante 
--la generaci ́on de la lista de todas las permutaciones de la lista original, y filtrando aquella que est ́a ordenada
slowsort1 :: Ord a => [a] -> [[a]]
slowsort1 [] = [[]]
slowsort1 xs = filter ordenada (permutaciones xs)

ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:y:ys) = (x<=y) && ordenada (y:ys)

--devuelve las subsecuencias de una lista
subsec :: [Int]->[[Int]]
subsec []=[[]]
subsec [x]=[[x]]
subsec (x:xs)= (aux (x:xs))++(subsec (xs))

aux ::[Int]->[[Int]]
aux []=[[]]
aux (x:xs)= (x:xs) :(aux (initm (x:xs)))

--esta funcion devuelve la subsecuencia de suma minima
sssm :: [Int]->Int
sssm []=0
sssm (x:xs)= minimo(sumalist(subsec (x:xs)))

--suma los elementos de una lista
sumaelist:: [Int]->Int
sumaelist [] = 0
sumaelist (x:xs) = x+sumaelist xs

--dada una lista de listas, devuelve una lista con la suma de los elementos de cada lista
sumalist::[[Int]]->[Int]
sumalist []=[]
sumalist (x:xs)= sumaelist x : sumalist xs 

--me devuelve el minimo de una lista
minimo :: [Int]->Int
minimo []=0
minimo [x]=x
minimo (x:y:xs)= if x<y then minimo (x:xs)
				else minimo (y:xs)  

--longitud de una lista
longitud::[Int]-> Int
longitud []=0
longitud (x:xs)= 1 + longitud xs

--ordena dos listas ordenadas en una
merge:: Ord a => [a]->[a]->[a]
merge xs[]=xs
merge []ys=ys
merge (x:xs)(y:ys)|x<y = x:merge xs (y:ys)
		      |y<=x = y:merge (x:xs) ys
--ordena una lista 
mergesort:: Ord a => [a] -> [a]
mergesort []=[]
mergesort [x]=[x]
mergesort (x:xs)= merge (mergesort (pm (x:xs))) (mergesort (sm (x:xs)))

--toma la primera mitad de un arreglo
pm :: [a]->[a]
pm []=[]
pm (xs)= take (div (length xs) 2) xs

--toma la segunda mitad de un arreglo
sm :: [a]->[a]
sm [] = []
sm (xs) = drop (div (length xs) 2) xs
