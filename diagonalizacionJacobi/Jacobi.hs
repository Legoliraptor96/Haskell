transpose m
  | not.null.head $ m = map head m : transpose (map tail m)
  | otherwise = []

--mult m n = map (\u-> map (\v-> sum $ zipWith (*) u v) nt) m
--  where nt = transpose n
mult m n = [[ sum $ zipWith (*) u v | v<-transpose n] | u<-m ]
--[f x | x<-xs, p x] === map f. filter p xs

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 a (l:ls) = a:ls
replace i a (l:ls) = l: replace (i-1) a ls

replaceM :: (Int,Int) -> a ->[[a]] ->[[a]]
replaceM (i,j) a m = replace i (replace j a (m!!i)) m

mkId n = [ replace i 1 (replicate n 0) | i<-[0..n-1]]

u a p q = replaceM (q,p) (-s) . replaceM (p,q) s . replaceM (q,q) c . replaceM (p,p) c $ mkId n
  where theta = (atan $ 2*((a!!p)!!q) / ((a!!q)!!q - (a!!p)!!p))/2
        c = cos theta
        s = sin theta
        n = length a

v a = multJacb [u a p q | p<-[0..n-1], q<-[p+1..n-1]]
  where n = length a

multJacb [u] = u
multJacb (u:us) = mult u $ multJacb us

maxNoD a = maximum [(a!!p)!!q | p<-[0..n-1], q<-[p+1..n-1]]
  where n = length a

jacobi e a
  | maxNoD a > e = jacobi e $ (transpose $ v a) `mult` a `mult` v a
  | otherwise = a
