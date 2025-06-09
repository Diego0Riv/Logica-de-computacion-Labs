module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Diego Rafael Rivero Moreira
-- Números: 269299
----------------------------------------------------

import Prelude
import Data.List

-- Formalización del lenguaje
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)
data BC = And | Or | Imp | Iff
  deriving (Eq)

-- Fórmulas del Lab1
p = V "p"
q = V "q"
r = V "r"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))


-- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i x
eval i (Neg x) = not (eval i x)
eval i (Bin f1 bc f2) 
  | bc == And = (eval i f1) && (eval i f2)
  | bc == Or = (eval i f1) || (eval i f2)
  | bc == Imp = not(eval i f1) || eval i f2
  | bc == Iff = (eval i f1) == (eval i f2)

--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas x = True

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas x = False

--1.4)
irfalsa :: Var -> Bool
irfalsa x
  | x=="r" = False
  | otherwise = True

--1.5)
-- Completar con verdadera/falsa:
-- fa es False bajo itodasfalsas
-- fb es False bajo itodasfalsas
-- fc es True bajo itodasfalsas
-- fd es False bajo itodasfalsas
-- 
-- fa es True bajo itodasverdaderas
-- fb es False bajo itodasverdaderas
-- fc es True bajo itodasverdaderas
-- fd es False bajo itodasverdaderas
--
-- fa es True bajo irfalsa
-- fb es False bajo irfalsa
-- fc es True bajo irfalsa
-- fd es False bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari [] x = error "Variable no encontrada D:"
creari ((v,b):ps) x
  | v == x = b
  | otherwise = creari ps x
--1.7)
-- Responder aquí.
-- Si, en el 4 la interpretacion dice que todas las variables a excepcion de r dan True, r dando False, lo mismo que en esta lista donde r
-- es la unica variable que da False

-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--Lab 1 listarprop formula
listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg x) = listarProp x
listarProp (Bin f1 bc f2) = nub (listarProp f1 ++ listarProp f2)


--2.1)
filas :: [Var] -> [Fila]
filas [] = error "Como llegaste aca :O?"
filas [x] =[[(x,b)] | b<-[False,True]]
filas ((x):xs) = [(x,b):fs | b<-[False,True], fs<-filas xs]

--2.2)
tv :: L -> TV
tv [] = error "Otra vez donde no deberias estar :/"
tv l =  

--2.3)
es :: L -> Clase -> Bool
es = undefined

--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es ...
-- fb es ...
-- fc es ...
-- fd es ...

--2.5) 
fnc :: L -> L
fnc = undefined


----------------------------------------------------------------------------------
-- Pretty Printing (rudimentario)
----------------------------------------------------------------------------------
instance Show L where
  show (V p)         = p
  show (Neg (Neg a)) = "¬" ++ show (Neg a)
  show (Neg (V p))   = "¬ " ++ show (V p)
  show (Neg a)       = "¬ (" ++ show a ++ ")"
  show (Bin a And b) = "(" ++ show a ++ ") /\\ (" ++ show b ++ ")"
  show (Bin a Or b)  = "(" ++ show a ++ ") \\/ (" ++ show b ++ ")"
  show (Bin a Imp b) = "(" ++ show a ++ ") --> (" ++ show b ++ ")"
  show (Bin a Iff b) = "(" ++ show a ++ ") <-> (" ++ show b ++ ")"