newton err f x = let x' = x - (f x)/(f' x) in if abs (x'-x) < err then x' else newton err f x'
  where f' x = aux $ map (\h-> (f (x+h) - f x)/h) [10**(-n) | n<-[1..]]
        aux (x:y:xs) = if abs (x-y) < err then y else aux (y:xs)

interpolNewton :: [(Double,Double)] -> (Double -> Double)
interpolNewton xys = \x-> sum [ (diffdiv [0..i]) * product [(x- xs!!j) | j<-[0..i-1]] | i<-[0.. length xys - 1] ]
  where (xs,ys) = (map fst xys, map snd xys)
        diffdiv [i] = ys!!i
        diffdiv is = (diffdiv (tail is) - diffdiv (init is))/(xs!!(last is)- xs!!(head is))
