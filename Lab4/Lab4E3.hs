
module Lab4E3 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

import SMTlib

type Nat = Int

----------------------------------------------------------------------------------
-- 3. Problema de la mochila
----------------------------------------------------------------------------------

-- 3.1. ... COMPLETAR CON RESPUESTA ...

-- 3.2. ... COMPLETAR CON RESPUESTA ...

-- 3.3. Formalización del Problema de la mochila 0-c
type KSc = (Nat,    -- (n)    Cantidad de objetos, identificados de 1 a n.
            [Nat],  -- (vs)   Lista de valores para cada objeto.
            [Nat],  -- (ws)   Lista de pesos para cada objeto.
            Nat,    -- (wmax) La capacidad máxima de peso.
            Nat,    -- (c)    Cantidad máxima que se puede llevar de cada objeto.
            Nat)    -- (v)    Valor total que se obtiene de realizar alguna selección de objetos.

-- Pre: recibe una instancia de problema de la mochila 0-c.
-- Pos: retorna una colección de fórmulas de LPO formalizando la instancia de problema
mochila0c :: KSc -> [LPO]
mochila0c = undefined

--      Problema de la mochila 0-c como problema de búsqueda.
-- Pre: recibe una instancia de problema de la mochila 0-c.
-- Pos: en caso positivo retorna el modelo que representa la seleccion, 
--      de lo contrario retorna Nothing.
solveMochila0c :: KSc -> IO (Maybe Model)
solveMochila0c = undefined

--      Problema de la mochila 0-c como problema de optimización.
-- Pre: recibe una instancia de problema de la mochila 0-c.
-- Pos: retorna una pareja con el valor maximo posible y el modelo que representa la seleccion
maxMochila0c :: KSc -> IO (Maybe (Int,Model))
maxMochila0c = undefined

-- 3.4. Formalización del Problema de la mochila 0-1
type KS1 = (Nat,    -- (n)    Cantidad de objetos, identificados de 1 a n.
            [Nat],  -- (vs)   Lista de valores para cada objeto.
            [Nat],  -- (ws)   Lista de pesos para cada objeto.
            Nat,    -- (wmax) La capacidad máxima de peso.
            Nat)    -- (v)    Valor total que se obtiene de realizar alguna selección de objetos. 

-- Pre: recibe una instancia de problema de la mochila 0-1.
-- Pos: retorna una colección de fórmulas de LPO formalizando la instancia de problema
mochila01 :: KS1 -> [LPO]
mochila01 = undefined
            
--      Problema de la mochila 0-1 como problema de búsqueda.
-- Pre: recibe una instancia de problema de la mochila 0-1.
-- Pos: en caso positivo retorna el modelo que representa la seleccion, 
--      de lo contrario retorna Nothing.
solveMochila01 :: KS1 -> IO (Maybe Model)
solveMochila01 = undefined

--      Problema de la mochila 0-1 como problema de optimización.
-- Pre: recibe una instancia de problema de la mochila 0-1.
-- Pos: retorna una pareja con el valor maximo posible y el modelo que representa la seleccion
maxMochila01 :: KS1 -> IO (Maybe (Int,Model))
maxMochila01 = undefined

-- 3.5. Problema de la mochila 0-inf
type KSinf = (Nat,    -- (n)    Cantidad de objetos, identificados de 1 a n.
              [Nat],  -- (vs)   Lista de valores para cada objeto.
              [Nat],  -- (ws)   Lista de pesos para cada objeto.
              Nat,    -- (wmax) La capacidad máxima de peso. 
              Nat)    -- (v)    Valor total que se obtiene de realizar alguna selección de objetos.       

-- Pre: recibe una instancia de problema de la mochila 0-inf.
-- Pos: retorna una colección de fórmulas de LPO formalizando la instancia de problema
mochila0Inf :: KSinf -> [LPO]
mochila0Inf = undefined

--      Problema de la mochila 0-inf como problema de búsqueda.
-- Pre: recibe una instancia de problema de la mochila 0-inf.
-- Pos: en caso positivo retorna el modelo que representa la seleccion, 
--      de lo contrario retorna Nothing.
solveMochila0Inf :: KSinf -> IO (Maybe Model)
solveMochila0Inf = undefined

--      Problema de la mochila 0-inf como problema de optimización.                 
-- Pre: recibe una instancia de problema de la mochila 0-inf.
-- Pos: retorna una pareja con el valor maximo posible y el modelo que representa la seleccion
maxMochila0Inf :: KSinf -> IO (Maybe (Int,Model))
maxMochila0Inf = undefined

-- 3.6. Resolver
-- a)
ma :: KS1
ma = undefined
-- ESCRIBIR SOLUCION AQUÍ

-- b)
mb :: KSinf 
mb = undefined
-- ESCRIBIR SOLUCION AQUÍ

-- c)
mc :: KSc
mc = undefined
-- ESCRIBIR SOLUCION AQUÍ

-- 3.7. Formalización del problema del subconjunto suma
type SS = ([Nat],  -- (xs)   Conjunto de números no negativos, aqui representado como lista.
           Nat)    -- (s)    Valor total que se obtiene de sumar algún subconjunto de los números.

-- Pre: recibe una instancia de problema de subconjunto suma
-- Pos: retorna una fórmula de LPO formalizando la instancia de problema
subsetSum :: SS -> [LPO]
subsetSum = undefined

--      Problema de subconjunto suma como problema de búsqueda.
-- Pre: recibe una instancia de problema de subconjunto suma.
-- Pos: en caso positivo retorna el modelo que representa la seleccion, 
--      de lo contrario retorna Nothing.
solveSubsetSum :: SS -> IO (Maybe Model)
solveSubsetSum = undefined

-- 3.8. Resolver
ssk1 :: SS
ssk1 = undefined
-- ESCRIBIR SOLUCION AQUÍ

ssk2 :: SS
ssk2 = undefined
-- ESCRIBIR SOLUCION AQUÍ

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