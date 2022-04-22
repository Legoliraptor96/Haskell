--Hanoii
import Data.List
import Data.Eq
data Torre= I | C | D
data Cambio = Mov Torre Torre


instance Show (Torre) where
                   show I = "Torre Izquierda"
                   show C = "Torre Central"
                   show D = "Torre Derecha"
                  -- deriving Eq

instance Eq Torre  where 
    I == I = True 
    C == C = True
    D == D = True
    _ == _ = False

instance Show (Cambio) where 
         show (Mov t1 t2) = "(" ++  "Mueve de " ++ show t1 ++ " a " ++ show t2 ++ ")"



--import Data.Char (intToDigit)
hanoi :: Int -> Cambio -> [Cambio]
hanoi 0 _  = []
--hanoi 1 (Mov I D)  = [Mov I D]
hanoi n (Mov t1 t2) = hanoi (n-1) (Mov t1 t3) ++ [(Mov t1 t2)] ++ hanoi (n-1) (Mov t2 t3) where    t3= head ([I,C,D]\\[t1,t2])                  
 --t3 = if t1 = I && t2=C then t3=D else if t1 = I && t2 = D then t3 = C else if t1=C &&

--hanoi n (Mov a b)  = hanoi (n-1) (Mov I D) ++ [(Mov I D)] ++ hanoi (n-1) (Mov C I)
--hanoi n _ _ _ = []
--hanoi n (Mov I C)  = hanoi (n-1) (Mov I D) C  ++ [(Mov I C)] ++ hanoi (n-1) (Mov C I) D 
--hanoi n a b c = hanoi (n-1) a c b ++ ["Mueve el disco " ++ [intToDigit n] ++ " de " ++ [a]++" a " ++ [c]] ++ hanoi (n-1) b a c 

--Arboles Binarios de Busqueda

--abb1 = crea (reverse[1..10])
--data  ABB a = Vacio
--            | Nodo a (ABB a) (ABB a)
--            deriving Eq

-- a = valores b= llave 

--data Arbol a b = Vacio | Rama a b (Arbol a b) (Arbol a b)
data Arbol a b = Vacio | Rama a b (Arbol a b) (Arbol a b)

instance (Show a, Show b, Ord a) => Show (Arbol a b) where
         show Vacio = " -"
         show (Rama a b i d) = "(" ++ show a ++ show b  ++ show i ++ show d ++ ")"

vacio :: Arbol a b
vacio = Vacio

--data Maybe a = Nothing | Just a
--pertenece :: (ord a,Show a) => a ->ABB a -> Maybe
busca  v' Vacio  = Nothing
busca  v' (Rama v l i d) | v==v' = Just l 
                         | v'<v = busca  v' i
                         | v'>v = busca  v' d


-- v' valor l' llave 
inserta v' l'  Vacio = Rama v' l' Vacio Vacio
inserta v' l'  (Rama v l i d) 
    | v' == v   = Rama v l i d
    | v' < v    = Rama v l (inserta v' l' i) d
    | otherwise = Rama v l i (inserta v' l'  d)

elimina v'  Vacio = Vacio 
elimina v'  (Rama v l i Vacio) | v'==v = i 
elimina v'  (Rama v l Vacio d) | v'==v = d
elimina v'  (Rama v l i d)
    | v'<v  = Rama v l (elimina v'  i) d 
    | v'>v  = Rama v l i (elimina v' d)  
    | v'==v = Rama k l i (elimina k d)
           where k = menor d 

menor (Rama v l Vacio _) = v 
menor (Rama _ _ i _)        = menor i 

arbol1 = Rama 1 'a' (Vacio) (Rama 2 'b' (Vacio)(Rama 3 'c' (Vacio) (Vacio) ) )

