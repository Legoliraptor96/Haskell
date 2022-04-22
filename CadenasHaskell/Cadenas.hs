charName 'a' = "Albert"
charName 'b' = "Broseph"

primero (a,_) = a

ts = [(x,y)| x<- [1..5], y <-[5..10]]

--[x+y|(x,y <- ts)]

head' (x:_) = x
head' [] = error "lista vacia no tiene cabeza"

capital "" = error "no hay primera letra"
--capital a@(x:xs) = "La primera letra de " ++ a ++ " es " ++ "" [x]

--bmiTell a p  
 -- | p / a ^ 2  <= 18.5 = "Estas bajo de peso"
 -- | p / a ^ 2  <= 30.0	= "Estas normal"
 -- | p / a ^ 2  <= 30.0 = "estas sobre peso"
 -- | otherwise   = "estas gordisimo"


bmiTell  p a
  | bmi <= skinny  = "Estas bajo de peso"
  | bmi <= normal  = "Estas normal"
  | bmi <= fat     = "estas sobre peso"
  | otherwise      = "estas gordisimo"
  where bmi = p / a ^ 2
        skinny = 18.5
        normal = 25.5
        fat = 30.0
iniciales nombre apellido = [n] ++ ". " ++ [a] ++ "."
    where (n:_) = nombre
          (a:_) = apellido

cilindro r h = let areaLado = 2 * pi * r * h ; areaTop = pi * r ^ 2 in areaLado + 2*areaTop 

head'' xs = case xs of [] -> error "descabezada"; (x:_) -> x

describe xs = "La lista esta " ++ case xs of [] -> "vacia"; _ -> "no vacia"

-- ejercicios clase 3

noRep :: Eq a => [a] -> [a]
noRep [] = []
noRep (x:xs) = if elem x (xs) then noRep xs else [x]++noRep(xs) -- === x:(noRep xs)
--noReap [1,1,2,2] -> [1,2]

dupll :: [a] -> [a]
dupll []=[]
dupll (x:xs) = [x,x]:dupll xs
-- dupll [1,2,3]   -> [1,1,2,2,3,3]

repl :: [a] -> [a]
repl [] = []
repl (x:xs) = [x,x,x]++repl(xs)
-- [1,2] -> [1,1,1,2,2,2]

take' :: Int -> [a] -> [a]
take' 0  [xs]  = []
take' i (x:xs) = [x] ++ take' (i-1) (xs)
--take' 3 [1,2,3,4] -> [1,2,3]

drop' :: Int -> [a] -> [a]
drop'  _  [] = []
drop'  i (x:xs) = if i>0 then drop' (i-1) (xs) else xs
--drop' 3 [1,2,3,4] -> [4]

--split :: Int -> [a] -> ([a],[a])
--split _ [] = []
--split i (x:xs) =   
--split 2 [1,2,3,4] = ([1,2],[3,4])

range :: Int -> Int -> [Int]
range i j = if i<=j then [i]++range (i+1) j else []
-- range 2 4  = [2,3,4]

maximo :: (Ord a) => [a] -> a
maximo [x]=x
maximo (x:y:xs) = if x<y then maximo(y:xs) else maximo(x:xs)
-- maximo [1,3,2] = 3

minimo :: (Ord a) => [a] -> a
minimo [x]=x
minimo (x:y:xs) = if x<y then minimo(x:xs) else minimo(y:xs)
-- [1,3,2] = 1

zip' :: [a] -> [b] -> [(a,b)]
zip' xs [] = []
zip' [] ys  = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
--zip' [1,2,3] "abc" = [(1,'a'),(2,'b'),(3,'c')]

quicksort :: (Ord a) => [a] -> [a]
quicksort []  = [] 
quicksort [x] = [x]
quicksort (a:b) = quicksort [x|x<-(a:b), x<a]++quicksort [x|x<-b, x >= a]
--quicksort [3,1,2] = [1,2,3]

