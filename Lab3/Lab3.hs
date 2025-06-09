module Lab3 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Diego Rafael Rivero Moreira
-- Números: 269299
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

----------------------------------------------------------------------------------
-- Formalización del lenguaje y otros elementos
----------------------------------------------------------------------------------
type Var = String
type I = [(Var,Bool)]

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)

data BC = And | Or | Imp | Iff 
  deriving (Show, Eq)

data Clase = Tau | Contra | Conti
  deriving (Show, Eq)

data Consecuencia = [L] :|= L deriving (Show, Eq)   

data Tableau = Conj [L] Tableau
             | Dis  [L] Tableau Tableau 
             | Hoja [L]
  deriving (Eq)
   
top = Bin (V "p") Or  (Neg $ V "p") 
bot = Bin (V "p") And (Neg $ V "p") 

-- 1)
-- Pre: recibe una lista de literales
-- Pos: retorna True si y solo si la lista es consistente, o sea no contiene un par de literales complementarios
esConsistente :: [L] -> Bool
esConsistente (V x:lits) = notElem (Neg (V x)) lits && esConsistente lits
esConsistente (Neg (V x):lits) = notElem ((V x)) lits && esConsistente lits


-- 2)
-- Pre: recibe una interpretación dada como lista de asignaciones (no vacía y consistente) 
-- Pos: retorna la interpretación expresada como una conjunción de literales
int2f :: I -> L
int2f [] = top

int2f [(x,b)]
  | b== True = V x
  | otherwise = (Neg (V x)) 

int2f ((x,b):f)
  | b==True = Bin (V x) And (int2f f)
  | otherwise = Bin (Neg (V x)) And (int2f f)

-- 3)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna el tableau de f
tableau :: L -> Tableau
tableau f = tableauRec [f]

tableauRec :: [L] -> Tableau
tableauRec (f:fs) = case f of
  
  Bin f1 And f2 -> Conj (f:fs) (tableauRec (f1:f2:fs))
  Neg (Bin f1 And f2) -> Conj (f:fs) (tableauRec (f1:Neg f2:fs))

  Neg (Neg f) -> Conj (f:fs) (tableauRec (f:fs))
  Bin f1 Or f2 -> Dis (f:fs) (tableauRec (f1:fs)) (tableauRec f2:fs)

  V x -> case allLit fs of
          True -> Hoja (f:fs)
          False -> tableauRec (fsNotLit fs:V x:(fs\\[fsNotLit fs]))

  Neg (V x) -> case allLit fs of
          True -> Hoja (f:fs)
          False -> tableauRec (fsNotLit fs:Neg (V x):(fs\\[fsNotLit fs]))

isLit :: L -> Bool
isLit (V x) = True
isLit (Neg (V x)) = True
isLit x = False

allLit :: [L] -> Bool
allLit fs = all isLit fs

fsNotLit :: [L] -> L
fsNotLit [] = error "No se encontro literales, literalmente"
fsNotLit (f:fs)
  | isLit f = fsNotLit fs
  | otherwise = f



-- 4)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna True si y solo si f es sat
sat :: L -> Bool
sat f = undefined
 
 
-- 5)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna una lista con todos los modelos completos de f
-- Recomendación: para imprimirlos los modelos en lineas distintas:
--                ghci> mapM_ print $ modelos f
modelos :: L -> [I]
modelos f = undefined 


combinaciones :: [Var] -> i
combinaciones [] = [[]]
combinaciones (x:xs) = [(x,b):cs | b <- [False, True], cs <- combinaciones xs]

hojas :: Tableau -> [I]
hojas (Hoja lits) = undefined --algo

listarprop :: L -> [Var]  --copiar de lab 1 o 2
listarprop (V x) = [x]
listarprop (Neg x) = listarprop x 
listarprop Bin f1 bin f2 = nub $ listarprop f1 ++ listarprop f2

faltantes :: I -> L -> [Var]  --dada la interpretacion I en L que variables faltan
faltantes=undefined

-- 6)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna la clase semántica a la que pertenece f
clasificar :: L -> Clase
clasificar f = undefined

-- 7)
-- Pre: recibe una consecuencia
-- Pos: retorna la consecuencia expresada como una fórmula de LP
cons2f :: Consecuencia -> L
cons2f cl = undefined

-- 8)     
-- Pre: recibe una consecuencia
-- Pos: retorna True si y solo si la consecuencia es válida
valida :: Consecuencia -> Bool
valida cl = undefined

-- 9)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FND
fnd :: L -> L
fnd f = undefined

-- 10)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FNC
fnc :: L -> L
fnc f = undefined


----------------------------------------------------------------------------------
-- Fórmulas del Lab1 para probar
----------------------------------------------------------------------------------
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


----------------------------------------------------------------------------------
-- Algunas funciones auxiliares 
----------------------------------------------------------------------------------
invertir :: L -> L
invertir f = (invertirVar (swapCon (dobleNeg f) And Or))
  where
    invertirVar (V p)= Neg (V p)
    invertirVar (Neg (V p)) = (V p)
    invertirVar (Neg f) = Neg (invertirVar f)
    invertirVar (Bin f1 o f2)  = Bin (invertirVar f1) o (invertirVar f2) 

swapCon :: L -> BC -> BC -> L
swapCon (V p) b1 b2 = V p
swapCon (Neg f) b1 b2 = Neg (swapCon f b1 b2)
swapCon (Bin f1 o f2) b1 b2 | (o == b1) = Bin (swapCon f1 b1 b2) b2 (swapCon f2 b1 b2)
                            | (o == b2) = Bin (swapCon f1 b1 b2) b1 (swapCon f2 b1 b2)
                            | otherwise = Bin (swapCon f1 b1 b2) o (swapCon f2 b1 b2)

vars :: L -> [Var]
vars (V p) = [p]
vars (Neg f) = vars f
vars (Bin f1 _ f2) = nub ((vars f1) ++ (vars f2))

dobleNeg :: L -> L
dobleNeg (V p) = V p
dobleNeg (Neg (Neg f)) = f
dobleNeg (Neg f) = Neg (dobleNeg f)
dobleNeg (Bin f1 o f2) = Bin (dobleNeg f1) o (dobleNeg f2)

deMorgan :: L -> L
deMorgan (V p) = V p
deMorgan (Neg (Bin f1 And f2)) = Bin (deMorgan $ Neg f1) Or  (deMorgan $ Neg f2)
deMorgan (Neg (Bin f1 Or f2))  = Bin (deMorgan $ Neg f1) And (deMorgan $ Neg f2)
deMorgan (Neg f) = Neg (deMorgan f)
deMorgan (Bin f1 o f2) = Bin (deMorgan f1) o (deMorgan f2)


----------------------------------------------------------------------------------
-- Pretty Printing
----------------------------------------------------------------------------------
instance Show L where
  show (V p)           = p
  show (Neg (Neg a))   = "¬" ++ show (Neg a)
  show (Neg (V p))     = "¬" ++ show (V p)
  show (Neg a)         = "¬" ++ show a ++ ""
  show (Bin a And b)   = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (Bin a Or b)    = "(" ++ show a ++ " \\/ " ++ show b ++ ")"
  show (Bin a Imp b)   = "(" ++ show a ++ " --> " ++ show b ++ ")"
  show (Bin a Iff b)   = "(" ++ show a ++ " <-> " ++ show b ++ ")"

instance Show Tableau where
    show = prettyPrintT  

-- Formatear tableau indentado
-- Adaptado de https://stackoverflow.com/a/19442407
prettyPrintT :: Tableau -> String
prettyPrintT t = unlines (prettyPrintAux t)
  where
    prettyPrintAux (Hoja i)       = [show i ++ if esConsistente i then " O" else " X"]
    prettyPrintAux (Conj l t)     = (show l) : prettyPrintSubTree [t]
    prettyPrintAux (Dis  l t1 t2) = (show l) : prettyPrintSubTree [t1,t2]
    --
    prettyPrintSubTree []     = []
    prettyPrintSubTree [t]    = ((pad "'- " "   ") (prettyPrintAux t))
    prettyPrintSubTree (t:ts) = ((pad "+- " "|  ") (prettyPrintAux t)) ++ prettyPrintSubTree ts
    --
    pad first rest = zipWith (++) (first : repeat rest)