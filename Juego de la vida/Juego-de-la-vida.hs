--juego de la vida
import Data.List
--Sorteo 
import System.Random
import Control.Monad.State 
--io
import System.IO

--main = gol
 
--gol = do 
--  raw <- readFile
--  vida 1 a

--     putStrLn ""
--     t <- promptInv "teclea el tablero con el formato [(x,y)] donde los elementos de la lista son las celuals vivas"
--     t = tablerojuego 
--     vida 1 tableroejemplo
-- Tablero 

type Pos = (Int,Int) -- monadas de posicion en lo alto y ancho del tablero 
type Tablero = [Pos] -- lista de posiciones


escribetablero t = sequence_ [escribir p "*"| p<- t] 

escribir :: Pos -> String -> IO ()
escribir p xs = do ira p
                   putStr xs

ira :: Pos -> IO ()
ira (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H") 

ejTablero :: Tablero
ejTablero = [(2,3),(3,4),(4,2),(4,3),(4,4)]

-- Vivos y muertoo  

vivo :: Tablero -> Pos -> Bool
vivo t p = elem p t 

muerto :: Tablero -> Pos -> Bool
muerto t p = not (vivo t p)

--vecinos
vecinos :: Pos -> [Pos]
vecinos (x,y) = map modular [(x-1,y-1), (x,y-1), (x+1,y-1), 
                             (x-1,y),            (x+1,y), 
                             (x-1,y+1), (x,y+1), (x+1,y+1)] 


modular :: Pos -> Pos 
modular (x,y) =(1+(x-1) `mod` ancho, 1+(y-1)`mod` alto)


nvecinosvivos :: Tablero -> Pos -> Int
nvecinosvivos t  = length. filter (vivo t) . vecinos

supervivientes :: Tablero -> [Pos]
supervivientes t = [ p | p<-t, elem (nvecinosvivos t p) [2,3]]

nacimientos :: Tablero -> [Pos]
nacimientos t = [(x,y) | x <- [1..ancho],y <- [1..alto], muerto t (x,y), nvecinosvivos t (x,y) == 3]

hijos :: Tablero -> [Pos]
hijos t = supervivientes t ++ nacimientos t

--espera entre generaciones
espera :: Int -> IO ()
espera n = sequence_ [return () | _ <- [1..n]]

-- limpiar pantalla 

limpiar :: IO()
limpiar = putStr "\ESC[2J"

vida :: Int -> Tablero -> IO ()
vida n t = do limpiar
              escribetablero t
              a <- getLine
              vida n (hijos t)

ancho :: Int
ancho = 5

alto :: Int
alto = 5


--sorteo (0,0)=()

--sorteo (x,y) gen = let ((x,y),newGen) = random gen in ecolor (x,y) gen :sorteo (pelotasrestantes (x,y) gen) newGen
sorteo (x,y) gen = do  ecolor (x,y) gen 
                   --str <- getLine ""
                  :sorteo (pelotasrestantes (x,y) gen) gen

ecolor (x,y) gen = if (colorpelota (x,y) gen) == True then "pelota roja" else "pelota azul" 

pelotasrestantes (x,y) gen = if colorpelota (x,y) gen == True then (x-1,y) else (x,y-1)

--tomapelota :: (RandomGen b, Random a) => (a, a) -> b -> a
--tomapelota (x,y) gen  = fst $ randomR (x,y) gen

colorpelota (x,y) gen = if (fst $ randomR (1,100) gen) <= (x/(x+y)*100) then True else False 

--colorpelota :: StdGen -> (Bool)
--colorpelota gen =
--        let (toma,newGen) = random gen
--        in  (toma)           


--randomSt ::  (RandomGen g, Random a) => State g a  
--randomSt = do
-- gen <- get
-- let (value,nextGen) = random gen
-- put nextGen
-- return value

--randomSt1 = (runState randomSt (mkStdGen 1)) :: (Int,StdGen)
--randomSt2 = (runState randomSt (mkStdGen 3)) :: (Float,StdGen)	

--sorteo 

--sorteo :: (Int,Int) -> State StdGen Bool
--sorteo (m,n) = 

--probt (m,n) = (m/(m+n))*10
--probf (m,n) = (n/(m+n))*10

--tub (m,n) =  if randomRIO (1,10) <= probt (m,n) then True else False


 
