--
-- lab 3.3
--
module DreiDrei where

-- 1 & 2
data Expr1 = Lit1 Integer | Add1 Expr1 Expr1 | Sub1 Expr1 Expr1 | Mul1 Expr1 Expr1 | Div1 Expr1 Expr1
eval1 :: Expr1 -> Integer
eval1 (Lit1 n) = n
eval1 (Add1 e1 e2) = eval1 e1 + eval1 e2
eval1 (Sub1 e1 e2) = eval1 e1 - eval1 e2
eval1 (Mul1 e1 e2) = eval1 e1 * eval1 e2
eval1 (Div1 e1 e2) = eval1 e1 `div` eval1 e2
size1 :: Expr1 -> Integer
size1 (Lit1 _) = 0
size1 (Add1 e1 e2) = 1 + size1 e1 + size1 e2
size1 (Sub1 e1 e2) = 1 + size1 e1 + size1 e2
size1 (Mul1 e1 e2) = 1 + size1 e1 + size1 e2
size1 (Div1 e1 e2) = 1 + size1 e1 + size1 e2

-- 3
data Expr2 = Lit2 Integer | Op Ops Expr2 Expr2 deriving (Eq, Show)
data Ops = Add2 | Sub2 | Mul2 | Div2 | Mod deriving (Eq, Show)
eval2 :: Expr2 -> Integer
eval2 (Lit2 n) = n
eval2 (Op Add2 e1 e2) = eval2 e1 + eval2 e2
eval2 (Op Sub2 e1 e2) = eval2 e1 - eval2 e2
eval2 (Op Mul2 e1 e2) = eval2 e1 * eval2 e2
eval2 (Op Div2 e1 e2) = eval2 e1 `div` eval2 e2
eval2 (Op Mod  e1 e2) = eval2 e1 `rem` eval2 e2
size2 :: Expr2 -> Integer
size2 (Lit2 _) = 0
size2 (Op _ e1 e2) = 1 + size2 e1 + size2 e2
