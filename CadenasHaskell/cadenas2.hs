menores :: Ord a => a -> [a] ->[a]
--menores 0 [xs] = []
menores i []   = []
--menores i (x:xs) = if x<i then [x]++menores i xs else menores i xs
menores i (x:xs) 
        | x<i       = [x]++menores i xs 
        | otherwise = menores i xs
--menores i [xs] = [x| x<-(xs), x<i]

mayores :: Ord a => a -> [a] ->[a]
--mayores 0 [xs] = [xs]
mayores i []   = []
--mayores i (x:xs) = if x>i then [x]++mayores i xs else mayores i xs
mayores i (x:xs)
        | x>i = [x]++mayores i xs
        | otherwise= mayores i xs


divisores:: Int -> [Int]
divisores n = [x| x <-[1..n-1], n `mod` x==0 ]

esPrimo :: Int-> Bool
esPrimo n = if divisores n == [1] then True else False

filtra  :: (a -> Bool ) -> [a] -> [a]
filtra funcion' [] = []
filtra funcion' (x:xs) = if funcion' x  then x:filtra funcion' (xs) else filtra funcion' (xs)
--filtra esPrimo [1..10] == [2,3,5,7]

takewhile' :: (a->Bool ) -> [a] -> [a]
--takewhile' i funcion' (x:xs) = menores i (filtra funcion' (x:xs))
takewhile' funcion' [] = []
takewhile' funcion' (x:xs) = if funcion' x then x:takewhile' funcion' xs else takewhile' funcion' []
--takewhile esPrimo [2..10]= [2,3]

dropwhile' :: (a->Bool) -> [a] -> [a]
dropwhile' funcion' [] = []
--dropwhile' esPrimo [2..10] = [4..10]
--dropwhile' funcion' (x:xs) = if funcion' x == False then x:dropwhile' funcion' xs else dropwhile' funcion' xs
dropwhile' funcion' (x:xs) = if funcion' x then dropwhile' funcion' xs else x:dropwhile' funcion' xs 
--dropwhile' funcion' (x:xs) = if funcion' x then dropwhile' funcion' [] else x:dropwhile' funcion' xs
