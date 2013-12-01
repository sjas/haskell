--
-- lab 3.4
--
module DreiVier where

data IExpr = ILit Integer 
           | ADD IExpr IExpr 
           | SUB IExpr IExpr 
           | MUL IExpr IExpr 
           | MOD IExpr IExpr 
           | IF BExp IExpr IExpr

-- 1
data BExp = BLit Bool
          | AND BExp BExp
          | OR  BExp BExp
          | NOT BExp
          | EQUAL IExpr IExpr

-- 2
iEval :: IExpr -> Integer
iEval (ILit n) = n
iEval (ADD e1 e2) = iEval e1 + iEval e2
iEval (SUB e1 e2) = iEval e1 - iEval e2
iEval (MUL e1 e2) = iEval e1 * iEval e2
iEval (MOD e1 e2) = iEval e1 `rem` iEval e2
iEval (IF b e1 e2) | bEval b = iEval e1
                   | otherwise = iEval e2

bEval :: BExp -> Bool
bEval (BLit a) = a
bEval (AND a b) = bEval a && bEval b
bEval (OR a b) = bEval a || bEval b
bEval (NOT a) = not (bEval a) 
bEval (EQUAL a b) = iEval a == iEval b
