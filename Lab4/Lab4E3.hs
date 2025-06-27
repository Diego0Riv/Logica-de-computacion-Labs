module Lab4E3 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Diego Rafael Rivero Moreira
-- Números: 269299
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

import SMTlib
import System.Posix.Internals (const_sig_block)

type Nat = Int

----------------------------------------------------------------------------------
-- 3. Problema de la mochila
----------------------------------------------------------------------------------

-- 3.1. ... COMPLETAR CON RESPUESTA ...
-- 0-1:   x_i ∈ {0, 1}
-- 0-c:   x_i ∈ {0, 1, ..., c}
-- 0-inf: x_i ∈ {0, 1, ...} 


-- 3.2. ... COMPLETAR CON RESPUESTA ...
-- Personalmente se me hace imposible no verlo desde un punto de vista matematico, y viendolo desde un punto de vista matematico puedo hacer limites, y hacer que c teinda a 1 o que tienda a infinito
-- En cuanto a Logica, creo que algo tal que (Para todo x)(Existe un y)(P(x,y)) y no hay ninguna restriccion diciendo que y no puede ser igual a x, asi como x puede ser cualquier numero desde 1 al infinito


-- 3.3. Formalización del Problema de la mochila 0-c
type KSc = (Nat,    -- (n)    Cantidad de objetos, identificados de 1 a n.
            [Nat],  -- (vs)   Lista de valores para cada objeto.
            [Nat],  -- (ws)   Lista de pesos para cada objeto.
            Nat,    -- (wmax) La capacidad máxima de peso.
            Nat,    -- (c)    Cantidad máxima que se puede llevar de cada objeto.
            Nat)    -- (v)    Valor total que se obtiene de realizar alguna selección de objetos.

-- Constantes: v_i, w_i, wmax, c, v
-- Variables: x_i

symbolsM0c :: KSc -> [SymDecl]
symbolsM0c (n, vs, ws, wmax, c, v) = vars ++ (mapConstDef "Int" consts)
  where
    vars   = genVars1 "Int" "x" [1..n]
    consts = [("c", show c), ("wmax", show wmax), ("v", show v)]
          ++ [("w" ++ show i, show (ws!!(i-1))) | i <- [1..n]]
          ++ [("v" ++ show i, show (vs!!(i-1))) | i <- [1..n]]

-- Regla A: La cantidad que me llevo de cada elemento esta entre 0 y c
--          -> 0 <= x_i <= c 

-- Regla B: La suma de valores es igual a v
--          -> sum (v_i * x_i) = v

-- Relga C: La suma de los pesos <= wmax 
--          -> sum (w_i * x_i) <= wmax

-- Pre: recibe una instancia de problema de la mochila 0-c.
-- Pos: retorna una colección de fórmulas de LPO formalizando la instancia de problema
mochila0c :: KSc -> [LPO]
mochila0c (n, vs, ws, wmax, c, v) =
  condA ++ [condB, condC]
  where

    condA = [ leq Zero xi `And` leq xi (C "c") | i <- [1..n], let xi = v1 "x" i ]

    condB = bigAdd [1..n] (\i -> ("v" ++ show i) `Mul` v1 "x" i) `Equ` C "v"

    condC = bigAdd [1..n] (\i -> ("w" ++ show i) `Mul` v1 "x" i) `leq` (C "wmax")

--      Problema de la mochila 0-c como problema de búsqueda.
-- Pre: recibe una instancia de problema de la mochila 0-c.
-- Pos: en caso positivo retorna el modelo que representa la seleccion, 
--      de lo contrario retorna Nothing.
solveMochila0c :: KSc -> IO (Maybe Model)
solveMochila0c ks = solve ("QF_LIA", symbolsM0c ks, map lpo2SMT (mochila0c ks))

--      Problema de la mochila 0-c como problema de optimización.
-- Pre: recibe una instancia de problema de la mochila 0-c.
-- Pos: retorna una pareja con el valor maximo posible y el modelo que representa la seleccion
maxMochila0c :: KSc -> IO (Maybe (Int,Model))
maxMochila0c (n, vs, ws, wmax, c, _) = maxMochila0c' (c * sum vs) -- el valor maximo que puedo obtener, llevarme c de todos los objetos
  where
    maxMochila0c' k
      | k < 0     = return Nothing
      | otherwise = do
        ans <- solveMochila0c (n, vs, ws, wmax, c, k)
        case ans of
          Nothing -> maxMochila0c' (k-1)
          Just m  -> return (Just (k,m))

-- 3.4. Formalización del Problema de la mochila 0-1
type KS1 = (Nat,    -- (n)    Cantidad de objetos, identificados de 1 a n.
            [Nat],  -- (vs)   Lista de valores para cada objeto.
            [Nat],  -- (ws)   Lista de pesos para cada objeto.
            Nat,    -- (wmax) La capacidad máxima de peso.
            Nat)    -- (v)    Valor total que se obtiene de realizar alguna selección de objetos. 

m01Tom0c :: KS1 -> KSc
m01Tom0c (n, vs, ws, wmax, v) = (n, vs, ws, wmax, 1, v)

-- Pre: recibe una instancia de problema de la mochila 0-1.
-- Pos: retorna una colección de fórmulas de LPO formalizando la instancia de problema
mochila01 :: KS1 -> [LPO]
mochila01 ks = mochila0c (m01Tom0c ks)

--      Problema de la mochila 0-1 como problema de búsqueda.
-- Pre: recibe una instancia de problema de la mochila 0-1.
-- Pos: en caso positivo retorna el modelo que representa la seleccion, 
--      de lo contrario retorna Nothing.
solveMochila01 :: KS1 -> IO (Maybe Model)
solveMochila01 = solveMochila0c . m01Tom0c

--      Problema de la mochila 0-1 como problema de optimización.
-- Pre: recibe una instancia de problema de la mochila 0-1.
-- Pos: retorna una pareja con el valor maximo posible y el modelo que representa la seleccion
maxMochila01 :: KS1 -> IO (Maybe (Int, Model))
maxMochila01 = maxMochila0c . m01Tom0c

-- 3.5. Problema de la mochila 0-inf
type KSinf = (Nat,    -- (n)    Cantidad de objetos, identificados de 1 a n.
              [Nat],  -- (vs)   Lista de valores para cada objeto.
              [Nat],  -- (ws)   Lista de pesos para cada objeto.
              Nat,    -- (wmax) La capacidad máxima de peso. 
              Nat)    -- (v)    Valor total que se obtiene de realizar alguna selección de objetos.       

m0InfTom0c :: KSinf -> KSc
m0InfTom0c (n, vs, ws, wmax, v) = (n, vs, ws, wmax, maxC, v)
  where
    maxC = wmax

-- Pre: recibe una instancia de problema de la mochila 0-inf.
-- Pos: retorna una colección de fórmulas de LPO formalizando la instancia de problema
mochila0Inf :: KSinf -> [LPO]
mochila0Inf = mochila0c . m0InfTom0c

--      Problema de la mochila 0-inf como problema de búsqueda.
-- Pre: recibe una instancia de problema de la mochila 0-inf.
-- Pos: en caso positivo retorna el modelo que representa la seleccion, 
--      de lo contrario retorna Nothing.
solveMochila0Inf :: KSinf -> IO (Maybe Model)
solveMochila0Inf = solveMochila0c . m0InfTom0c

--      Problema de la mochila 0-inf como problema de optimización.                 
-- Pre: recibe una instancia de problema de la mochila 0-inf.
-- Pos: retorna una pareja con el valor maximo posible y el modelo que representa la seleccion
maxMochila0Inf :: KSinf -> IO (Maybe (Int, Model))
maxMochila0Inf = maxMochila0c . m0InfTom0c



-- 3.6. Instancias para el problema de la mochila

-- Lista de objetos (valor, peso)
vs3_6 :: [Nat]
vs3_6 = [12,8,2,15,5,22,9,13,7,24]

ws3_6 :: [Nat]
ws3_6 = [3,2,5,3,1,6,2,3,2,4]

-- (a) Mochila 0-1
ma :: KS1
ma = (10, vs3_6, ws3_6, 15, 0)

-- (b) Mochila 0-inf
mb :: KSinf
mb = (10, vs3_6, ws3_6, 15, 0)

-- (c) Mochila 0-c con c = 2
mc :: KSc
mc = (10, vs3_6, ws3_6, 15, 2, 0)

-- Funcion para resolver los tres casos
resolver3_6 :: IO ()
resolver3_6 = do
  putStrLn "\n[MOCHILA 0-1]"
  res1 <- maxMochila01 ma
  print res1

  putStrLn "\n[MOCHILA 0-INF]"
  res2 <- maxMochila0Inf mb
  print res2

  putStrLn "\n[MOCHILA 0-C con c = 2]"
  res3 <- maxMochila0c mc
  print res3
--La salida es una lista de pares tal que ("xn","cantidad"), la n es el objeto, es decir el primer objeto de la tabla dada mientras que la cantidad, bueno, es la cantidad de ese objeto

-- 3.7. Formalización del problema del subconjunto suma
type SS = ([Nat], Nat)

-- Pre: recibe una instancia del problema de subconjunto suma
-- Pos: retorna una colección de fórmulas LPO representando la instancia
subsetSum :: SS -> [LPO]
subsetSum (xs, s) =
  reglaA : [reglaB]
  where
    n = length xs
    reglaA = bigAnd [1..n] (\i -> (v1 "x" i) `leq` One) 
    reglaB = bigAdd [1..n] (\i -> show (xs !! (i-1)) `Mul` v1 "x" i) `Equ` C (show s)

-- Resolver subconjunto suma
solveSubsetSum :: SS -> IO (Maybe Model)
solveSubsetSum ss@(xs, s) =
  solve ("QF_LIA", symbolsSS ss, map lpo2SMT (subsetSum ss))

-- Declaraciones de simbolos para subconjunto suma
symbolsSS :: SS -> [SymDecl]
symbolsSS (xs, s) = genVars1 "Int" "x" [1..n] ++ mapConstDef "Int" consts
  where
    n = length xs
    consts = [("s", show s)] ++ [("a" ++ show i, show (xs !! (i-1))) | i <- [1..n]]


-- 3.8. Instancias del problema del subconjunto suma
listaSS :: [Nat]
listaSS = [42, 18, 24, 78, 12, 66, 2, 14, 50, 34, 30, 28, 56, 6, 10]

ssk1 :: SS
ssk1 = (listaSS, 84)

ssk2 :: SS
ssk2 = (listaSS, 95)

resolver3_8 :: IO ()
resolver3_8 = do
  putStrLn "\n[SUBSET SUM k = 84]"
  r1 <- solveSubsetSum ssk1
  print r1

  putStrLn "\n[SUBSET SUM k = 95]"
  r2 <- solveSubsetSum ssk2
  print r2

--La salida es similar a la de resolver3_6, es una lista de pares ("xn","cantidad") el n representa el numero en la posicion de la lista  listaSS, viendolo como un array tradicional
--mientras que la cantidad es la cantidad de ese numero

-- 3.9*. (Opcional)


----------------------------------------------------------------------------------
-- Definiciones básicas para un lenguaje de LPO interpretado en la aritmética
----------------------------------------------------------------------------------

type Var   = String
type Const = String

data Term = Zero | One | V Var | C Const | Add Term Term | Sub Term Term | Mul Const Term
  deriving (Eq)

data LPO = Top | Equ Term Term | Lq Term Term | Neg LPO | And LPO LPO | Or LPO LPO 
  deriving (Eq)

-- "x <= y" es una abreviación para "x < y \/  x = y" 
leq x y = (x `Lq` y) `Or` (x `Equ` y)
bot = Neg Top 

-- Conjuntoria (universal finito) de fórmulas indexadas
bigAnd :: [Int] -> (Int -> LPO) -> LPO
bigAnd is f = foldr (\i b -> And (f i) b) Top is

-- Sumatoria de fórmulas indexadas
bigAdd :: [Int] -> (Int -> Term) -> Term
bigAdd is f = foldr (\i b -> Add (f i) b) Zero is

-- Utilidad para construir variables indexadas
v1 :: Var -> Nat -> Term
v1 name i = V (name ++ (show i))

-- Utilidad para construir constantes indexadas
c1 :: Const -> Nat -> Term
c1 name i = C (name ++ (show i))

-- Traducción de LPO a SMT-LIB

-- Pre: recibe un ambiente de traducción para constantes y un término de LPO.
-- Pos: convierte el término al formato SMT-LIB.
term2SMT :: Term -> String
term2SMT t = term2SMT' t 
  where 
    term2SMT' Zero        = "0"
    term2SMT' One         = "1"
    term2SMT' (V v)       = v
    term2SMT' (C c)       = c
    term2SMT' (Add t1 t2) = "(+ " ++ term2SMT' t1 ++ " " ++ term2SMT' t2 ++ ")"   
    term2SMT' (Sub t1 t2) = "(- " ++ term2SMT' t1 ++ " " ++ term2SMT' t2 ++ ")"  
    term2SMT' (Mul c  t2) = "(* " ++ c ++ " " ++ term2SMT' t2 ++ ")"     

-- Pre: recibe una fórmula de LPO.
-- Pos: traduce la fórmula al formato SMT-LIB.
lpo2SMT :: LPO -> String
lpo2SMT f = lpo2SMT' f 
  where 
    lpo2SMT' Top         = "true"    
    lpo2SMT' (Equ t1 t2) = "(= "   ++ term2SMT t1 ++ " " ++ term2SMT t2 ++ ")"    
    lpo2SMT' (Lq  t1 t2) = "(< "   ++ term2SMT t1 ++ " " ++ term2SMT t2 ++ ")"   
    lpo2SMT' (Neg a)     = "(not " ++ lpo2SMT a ++ ")"
    lpo2SMT' (And a  b)  = "(and " ++ lpo2SMT  a  ++ " " ++ lpo2SMT  b ++ ")"  
    lpo2SMT' (Or (Lq x y) (Equ x' y')) | x == x' && y == y' = 
                           "(<= " ++ term2SMT x  ++ " " ++ term2SMT y ++ ")"    
    lpo2SMT' (Or  a  b)  = "(or "  ++ lpo2SMT  a  ++ " " ++ lpo2SMT  b ++ ")"   

-- Pretty printing de LPO en consola

instance Show Term where
  show Zero        = "0"
  show One         = "1"
  show (V v)       = v
  show (C c)       = c
  show (Add t1 t2) = "(" ++ show t1 ++ " + " ++ show t2 ++ ")"    
  show (Sub t1 t2) = "(" ++ show t1 ++ " - " ++ show t2 ++ ")"    
  show (Mul c  t2) = "(" ++ show c ++ " * " ++ show t2 ++ ")"  

instance Show LPO where
  show Top          = "T"    
  show (Equ t1 t2)  = "(" ++ show t1 ++ " = "   ++ show t2 ++ ")"    
  show (Lq  t1 t2)  = "(" ++ show t1 ++ " < "  ++ show t2 ++ ")"  
  show (Neg a)      = "(¬ " ++ show a ++ ")"  
  show (And a  b)   = "(" ++ show a  ++ " /\\ " ++ show b  ++ ")"  
  show (Or (Lq x y) (Equ x' y')) | x == x' && y == y' = 
                      "(" ++ show x  ++ " <= " ++ show y  ++ ")"  
  show (Or a  b)    = "(" ++ show a  ++ " \\/ " ++ show b  ++ ")"



voc :: [SymDecl]
voc = mapConstDef "Int" consts  -- ponemos constantes
   ++ mapVarDecl "Int" vars     -- ponemos variables
  where
    consts = [("a1","3"), ("a2","2"), ("c","7")]
    vars   = ["x", "y"]

form :: LPO                     -- Formula:
form = lhs `leq` Zero           -- 3x + 2y - 7 <= 0
  where
    lhs = ("a1" `Mul` (V "x")) `Add` ("a2" `Mul` (V "y")) `Sub` (C "c")

-- solve (" QF_LIA", voc , [lpo2SMT form])