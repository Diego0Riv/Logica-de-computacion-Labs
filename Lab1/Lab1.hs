module Lab1 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Diego Rafael Rivero Moreira
-- Números: 269299
----------------------------------------------------

import Prelude
import Data.List

-- EJERCICIO 1.1 --
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Show, Eq)
data BC = And | Or | Imp | Iff
  deriving (Show, Eq)
  
  
-- EJERCICIO 1.2 --

p = V "p"

q = V "q"

r = V "r"
--a)
fa :: L
fa = Bin (p) And (Neg (Neg (q)))
--b):
fb :: L
fb = Bin (Bin (p) And (Neg(q))) And (Neg(r))
--c)
fc :: L
fc = Bin (Neg(Neg(p))) Or (Bin (q) And (p))
--d)
fd :: L
fd = Bin (Neg(Bin (r) Imp (r))) And (Bin (Neg(Neg(p))) Or (Neg(Bin (q) And (p))))


-- EJERCICIO 1.3 --
--a)
cantBin :: L -> Int
cantBin (V x) = 0
cantBin (Neg f) = 0 + (cantBin f)
cantBin (Bin f1 bc f2) = 1 + cantBin f1 + cantBin f2

--b)
valores :: L -> [(Var,Bool)]
valores (V x) = [(x, True)]
valores (Neg (V x)) = [(x, False)]
valores (Bin f1 bc f2) = (valores f1)++(valores f2)

--c)
dobleNeg :: L -> L
dobleNeg (V x) = V x
dobleNeg (Neg (Neg x)) = x
dobleNeg (Neg x) = Neg x
dobleNeg (Bin f1 bc f2) = Bin (dobleNeg f1) bc (dobleNeg f2)

--d)
cambiar :: L -> L
cambiar (V x) = V x
cambiar (Neg x) = Neg x
cambiar (Bin f1 Or f2) = Bin (Neg (cambiar f1)) Imp (cambiar f2)
cambiar (Bin f1 bc f2) = Bin (cambiar f1) bc (cambiar f2)

--e)
cantPropX :: L -> Var -> Int
cantPropX (V x) str = if x==str then 1 else 0
cantPropX (Neg x) str = cantPropX x str
cantPropX (Bin f1 bc f2) str = cantPropX f1 str + cantPropX f2 str

--f)
listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg x) = listarProp x
listarProp (Bin f1 bc f2) = nub (listarProp f1 ++ listarProp f2)


--g)
sustCon :: L -> BC -> BC -> L
sustCon (V x) b1 b2 = V x
sustCon (Neg x) b1 b2 = Neg (sustCon x b1 b2)
sustCon (Bin f1 bc f2) b1 b2
  | bc == b1  = Bin (sustCon f1 b1 b2) b2 (sustCon f2 b1 b2)
  | otherwise =  Bin (sustCon f1 b1 b2) bc (sustCon f2 b1 b2) 


--h)
swapCon :: L -> BC -> BC -> L
swapCon (V x) b1 b2 = V x
swapCon (Neg x) b1 b2 = Neg (swapCon x b1 b2)
swapCon (Bin f1 bc f2) b1 b2
  | bc == b1  = Bin (swapCon f1 b1 b2) b2 (swapCon f2 b1 b2)
  | bc == b2  = Bin (swapCon f1 b1 b2) b1 (swapCon f2 b1 b2)
  | otherwise =  Bin (swapCon f1 b1 b2) bc (swapCon f2 b1 b2) 

--i)
invertir :: L -> L
invertir (V x) = Neg (V x)
invertir (Neg x) = dobleNeg(Neg (invertir x))
invertir (Bin f1 bc f2) = swapCon(Bin 
                                    (invertir (swapCon f1 And Or)) 
                                    bc 
                                    (invertir (swapCon f2 And Or))
                                  ) And Or

--j)
sustSimp :: Var -> L -> L -> L
sustSimp str y (V x) = if x==str then y else V x
sustSimp str y (Neg x) = Neg (sustSimp str y x)
sustSimp str y (Bin f1 bc f2) = Bin (sustSimp str y f1) bc (sustSimp str y f2)

--k)
sustMult :: [(Var, L)] -> L -> L
sustMult [(str, y)] (V x) = if x == str then y else V x
sustMult ((str, y):xs) (V x) = if x ==str then y else sustMult xs (V x)
sustMult s (Neg x) = Neg(sustMult s x)
sustMult s (Bin f1 bc f2) = Bin (sustMult s f1) bc (sustMult s f2)