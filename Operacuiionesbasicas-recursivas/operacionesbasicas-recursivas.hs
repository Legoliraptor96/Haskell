
fact 0 = 1
fact n = n * fact(n-1)

suma :: Int-> Int-> Int
suma x 0 = x
suma 0 y = y
suma x y = succ(suma x (y-1))

mult :: Int-> Int-> Int
mult x 1 = x
mult 1 y = y
mult x y = suma x  (mult x (y-1)) 

divisores:: Int -> [Int]
divisores n = [x| x <-[1..n-1], n `mod` x==0 ]

divisores2:: Int -> [Int]
divisores2 n = [x| x <-[2..n-1], n `mod` x==0 ]

esPrimo :: Int-> Bool
esPrimo n = if divisores n == [1] then True else False 

mcd :: Int-> Int-> Int -- maximo comun divisor
mcd x 0  = x
mcd x y = mcd y (x `mod` y) 

--mcm :: Int-> Int-> Int -- maximo comun multiplo

coPrimos :: Int-> Int-> Bool
coPrimos x y = if interseccion (divisores2 (x)) (divisores2 (y)) /= [] then True else False

ultimo :: [a]-> a
ultimo [a]=a
ultimo (a:lista)= ultimo (lista)

penultimo :: [a]-> a
penultimo [a,b] = a
penultimo (a:lista)=penultimo (lista)

--elemeni  :: [a] -> Int -> a -- i es el indice no el elemento 
--elemeni  :: [a]-> a
--elemeni 0  [a:lista] = a
--elemeni i  [a:lista] = elemeni (i-1) (lista)
--elemeni  i (a:lista) = if elemeni i (a,lista) == i then (a,lista) = a else 1+elemeni i (lista)

longitud   :: [a] -> Int
longitud  [] = 0
longitud (a:lista) = 1 + longitud (lista)

reversa    :: [a] -> [a]
reversa [] = []
reversa (a:lista) = reversa (lista) ++ [a]

palindromo (lista) = if lista == reversa lista  then True else False

--union             :: [a]->[a]->[a]
--union (xs) (ys) = if 

--interseccion      :: [a]->[a]->[a]
interseccion (xs) (ys) = [x | x <- xs, x `elem` ys ]

