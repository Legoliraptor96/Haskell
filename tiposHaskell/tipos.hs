



--type String = [Char]

type Pt2 = (Float,Float)

dist :: Pt2 -> Pt2 -> Float
dist (x,y) (x',y') = sqrt $ (x-x')^2 + (y-y')^2

type Trans = Pt2 -> Pt2

inv1 :: Trans
inv1 (x,y) = (-x,y)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v)<-t, k==k']

data Bool' = False' | True'

not' :: Bool' -> Bool'
not' True' = False'
not' _ = True'

data Dir = N | S | W | E
move :: Dir -> Trans
move N = \(x,y)->(x,y+1)
move S = \(x,y)->(x,y-1)
move W = \(x,y)->(x-1,y)
move E = \(x,y)->(x+1,y)

--data Maybe' a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv a b = Just $ div a b

fromDiv :: Maybe Int -> Int
fromDiv Nothing = -1
fromDiv (Just n) = n

--data Either' a b = Left a | Right b
safediv1 :: Int -> Int -> Either String Int
safediv1 _ 0 = Left "division entre 0"
safediv1 a b = Right $ div a b

data Nat = Z | S' Nat deriving Show

nat2int :: Nat -> Int
nat2int Z = 0
nat2int (S' n) = succ $ nat2int n

int2nat :: Int -> Nat
int2nat 0 = Z
int2nat n = S' $ int2nat (n-1)

suma :: Nat -> Nat -> Nat
suma n Z = n
suma n (S' m) = suma (S' n) m

data List a = Nil | Cons a (List a) deriving Show

safeCabeza :: List a -> Maybe a
safeCabeza Nil = Nothing
safeCabeza (Cons a _) = Just a

cola Nil = Nil
cola (Cons _ l) = l

len :: List a -> Int
len Nil = 0
len (Cons a as) = 1+ len as

data Arbol a = Hoja a | Rama a (Arbol a) (Arbol a)

arbol1 = Rama 5 (Rama 3 (Hoja 1) (Hoja 4)) (Rama 7 (Hoja 6) (Hoja 9))

contiene :: Eq a => a -> Arbol a -> Bool
contiene a (Hoja a') = a==a'
contiene a (Rama a' rI rD) = a==a' || contiene a rI || contiene a rD

aplana :: Arbol a -> [a]
aplana (Hoja a) = [a]
aplana (Rama a ri rd) = aplana ri ++ [a] ++ aplana rd
