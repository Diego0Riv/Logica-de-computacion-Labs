
module Lab4 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Diego Rafael Rivero Moreira
-- Números: 269299
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

import SMTlib

type Nat = Int

----------------------------------------------------------------------------------
-- 1. Veraces y mentirosos
----------------------------------------------------------------------------------

-- Declaración de letras proposicionales e interpretación intuitiva:
--    ... COMPLETAR AQUÍ ...
vocEj1 :: [SymDecl]
vocEj1 = [varDecl "Bool" "a",
          varDecl "Bool" "b",
          varDecl "Bool" "c"]

a =  V "a"
b = V "b"
c = V "c" 

-- Formalización del caso 1.
-- Respuesta: ... COMPLETAR CON RESPUESTA ...
caso1 :: [L] -- and de formulas
caso1 = [Bin a Iff diceA, Bin b Iff diceB]
  where
    diceA = Bin (Neg a) And (Bin (Neg b) And (Neg c))
    diceB = (a `myAnd` (Neg b) `myAnd` (Neg c)) `myOr` (
      ((Neg a) `myAnd` (b) `myAnd` (Neg c))
       `myOr`
      ((Neg a) `myAnd` (Neg b) `myAnd` (c))) 

satProblemCaso1 :: SATproblem
satProblemCaso1 = ("QF_LIA", vocEj1, map lp2SMT caso1)



-- Formalización del caso 2.
-- Respuesta: ... COMPLETAR CON RESPUESTA ...
caso2 :: [L]
caso2 = [Bin a Iff diceA, Bin b Iff diceB1, Bin b Iff diceB2, Bin c Iff diceC]
  where
    diceA = Bin (Bin (b) And (Neg c)) Or (Bin (Neg b) And (c))
    diceB1 = Bin (Neg a) And (Neg b)
    diceB2 = Neg c
    diceC = a

satProblemCaso2 :: SATproblem
satProblemCaso2 = ("QF_LIA", vocEj1, map lp2SMT caso2)

----------------------------------------------------------------------------------
-- 2. Binairo y Tango
----------------------------------------------------------------------------------

-- 2.1 Formalización de Binairo básico   
type Binairo = (Nat,           -- (n)  Tamaño del tablero, tal que n >= 4 y n par
                [(Nat,Nat)],   -- (ss) Celdas con sol 
                [(Nat,Nat)])   -- (ls) Celdas con luna 

binairo :: Binairo -> [L]
binairo b = [initialState b,
             condJustOne b,
             condA b,
             condB b]

condJustOne :: Binairo -> L
condJustOne (n, _, _) =
  bigAnd [1..n] (\i ->
    bigAnd [1..n] (\j ->
        Neg (Bin (v2 "s" i j) Iff (luna2LP (i, j)))
      )
    )

condA :: Binairo -> L
condA b = condAFilas b `myAnd` condACols b

condAFilas :: Binairo -> L
condAFilas (n, _, _) =
  bigAnd [1..n] (\i ->
      (exactK (n `div` 2) [1..n] (\j ->
          v2 "s" i j
        )) 
      `myAnd` 
      (exactK (n `div` 2) [1..n] (\j ->
          v2 "l" i j
        ))
    )

condACols :: Binairo -> L
condACols (n, _, _) =
  bigAnd [1..n] (\j ->
      (exactK (n `div` 2) [1..n] (\i ->
          v2 "s" i j
        )) 
      `myAnd` 
      (exactK (n `div` 2) [1..n] (\i ->
          v2 "l" i j
        ))
    )

condB :: Binairo -> L
condB b = condBFilas b `myAnd` condBCols b

condBFilas :: Binairo -> L
condBFilas (n, _, _) =
  bigAnd [1..n] (\i ->
      (Neg (existSeq3 [1..n] (\j ->
          v2 "s" i j
        ))) 
      `myAnd` 
      (Neg (existSeq3 [1..n] (\j ->
          v2 "l" i j
        )))
    )

condBCols :: Binairo -> L
condBCols (n, _, _) =
  bigAnd [1..n] (\j ->
      (Neg (existSeq3 [1..n] (\i ->
          v2 "s" i j
        ))) 
      `myAnd` 
      (Neg (existSeq3 [1..n] (\i ->
          v2 "l" i j
        )))
    )

existSeq3 :: [Nat] -> (Nat -> L) -> L
existSeq3 is f =
  bigOr is (\n ->
      (f n) `myAnd` (bigOr (nextTo n) (\n' ->
          (f n') `myAnd` (bigOr (nextTo n') (\n'' ->
              f n''
            ))
        ))
    )
  where
    nextTo y = [ x | x <- is, x == y + 1]


exactK :: Nat -> [Nat] -> (Nat -> L) -> L
exactK 1 is f =
  bigOr is (\i ->
    (f i) `myAnd` (Neg (
      bigOr (is\\[i]) (\i' ->
          f i'
        )
    ))
    )  

exactK k is f =
  bigOr is (\i ->
    (f i) `myAnd` (exactK (k-1) (is\\[i]) f)
    )

initialState :: Binairo -> L
initialState (n, soles, lunas) = Bin (conj $ map sol2LP soles) 
                                 And (conj $ map luna2LP lunas)

conj :: [L] -> L
conj [] = top
conj (f:fs) = f `myAnd` (conj fs)

sol2LP :: (Nat, Nat) -> L
sol2LP (i, j) = v2 "s" i j

luna2LP :: (Nat, Nat) -> L
luna2LP (i, j) = v2 "l" i j

vocBinairo :: Binairo -> [SymDecl]
vocBinairo (n, _, _) = genVars2 "Bool" "s" [1..n] [1..n]
                    ++ genVars2 "Bool" "l" [1..n] [1..n]
                    ++ [pVarDec]

-- Resolución del puzzle
solveBinairo :: Binairo -> IO (Maybe Model)
solveBinairo b = solve ("QF_LIA", vocBinairo b, map lp2SMT (binairo b))

-- 2.2 Resolver Binairo básico b_n8
b_n8 :: Binairo  
b_n8 = (8, [(1,6),(1,8),(3,8),(4,6),(5,1),(5,2),(5,6),(6,1),(8,7)], 
           [(1,1),(1,3),(2,3),(2,5),(3,4),(4,4),(7,6)])

-- 2.3 Formalización de Binairo estándar
binairoStd :: Binairo -> [L]
binairoStd = undefined                   

-- Resolución del puzzle
solveBinairoStd :: Binairo -> IO (Maybe Model)
solveBinairoStd = undefined

-- 2.4 Resolver Binairo std b_n8 y comparar con soluciones del punto 2.2
-- ... EXPLICAR AQUÍ ...

-- 2.5 Formalización de Tango  
data Cond  = Eq | Neq
type Tango = (Nat,                            -- (n)  Tamaño del tablero, tal que n >= 4 y n par
              [(Nat,Nat)],                    -- (ss) Celdas con sol 
              [(Nat,Nat)],                    -- (ls) Celdas con luna 
              [((Nat,Nat),(Nat,Nat), Cond)])  -- (cs) Condiciones de adyacencia

tango :: Tango -> [L]
tango = undefined

-- Resolución del puzzle
solveTango :: Tango -> IO (Maybe Model)
solveTango = undefined

-- 2.6 Resolver Tango t_n8
t_n8 :: Tango 
t_n8 = (8, [(2,6),(4,3),(5,5),(6,2),(8,2),(8,7)], 
           [(1,3),(1,6),(2,2),(2,3),(2,7),(4,6),(5,4),(6,7)], 
           [((3,5),(3,6),Eq),  ((6,4),(6,5),Eq),  ((7,7),(7,8),Eq),
            ((1,5),(2,5),Neq), ((5,1),(6,1),Neq), ((6,3),(7,3),Neq)])    


----------------------------------------------------------------------------------
-- Definiciones básicas para LP
----------------------------------------------------------------------------------
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)

data BC = And | Or | Imp | Iff 
  deriving (Show, Eq)

-- Top y Bottom se definen en términos de las conectivas primitivas
top = Bin (V "p") Or  (Neg $ V "p") 
bot = Bin (V "p") And (Neg $ V "p") 

-- Conjuntoria (universal finito) de fórmulas indexadas
bigAnd :: [Int] -> (Int -> L) -> L
bigAnd [] _ = top
bigAnd (i:is) f = Bin (f i) And (bigAnd is f) 

-- Disyuntoria (existencial finito) de fórmulas indexadas
bigOr :: [Int] -> (Int -> L) -> L
bigOr [] _ = bot
bigOr (i:is) f = Bin (f i) Or (bigOr is f)

-- Utilidad para construir variables proposicionales indexadas
v1 :: Var -> Nat -> L
v1 p i = V (p ++ (show i))

-- Utilidad para construir variables proposicionales doblemente indexadas
v2 :: Var -> Nat -> Nat -> L
v2 p i j = V (p ++ (show i) ++ "_" ++ (show j))

-- Utilidad para construir variables proposicionales triplemente indexadas
v3 :: Var -> Nat -> Nat -> Nat -> L
v3 p i j k = V (p ++ (show i) ++ "_" ++ (show j) ++ "_" ++ (show k))

-- Para escribir conectivas anidadas más facil 
myOr  a1 a2 = Bin a1 Or  a2   
myAnd a1 a2 = Bin a1 And a2 
myImp a1 a2 = Bin a1 Imp a2 

-- Traducción de LP a SMT-LIB

pVarDec = "(declare-fun p () Bool)\n"

-- Pre: recibe una fórmula de LP.
-- Pos: traduce la fórmula al formato SMT-LIB.
lp2SMT :: L -> String
lp2SMT (V p)         = p
lp2SMT (Neg a)       = "(not " ++ lp2SMT a ++ ")"
lp2SMT (Bin a And b) = "(and " ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"
lp2SMT (Bin a Or  b) = "(or "  ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"
lp2SMT (Bin a Imp b) = "(=> "  ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"
lp2SMT (Bin a Iff b) = "(= "   ++ lp2SMT a ++ " " ++ lp2SMT b ++ ")"     

-- Pretty printing de LP en consola

instance Show L where
  show (V p)         = p
  show (Neg a)       = "¬(" ++ show a ++ ")"
  show (Bin (V p) And (Neg (V p'))) | p == p' =  "_|_"
  show (Bin a And b) = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (Bin (V p) Or (Neg (V p'))) | p == p' = "T"
  show (Bin a Or  b) = "("  ++ show a ++ " \\/ " ++ show b ++ ")"
  show (Bin a Imp b) = "("  ++ show a ++ " => " ++ show b ++ ")"
  show (Bin a Iff b) = "("   ++ show a ++ " <=> " ++ show b ++ ")" 