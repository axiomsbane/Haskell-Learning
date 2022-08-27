module Calc where
import ExprT
import Parser (parseExp)

-- data ExprT = Lit Integer
--            | Add ExprT ExprT
--            | Mul ExprT ExprT
--   deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit val) = val
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

go :: Maybe ExprT -> Maybe Integer
go (Just exp) = Just (eval exp)
go Nothing = Nothing 

evalStr :: String -> Maybe Integer
evalStr = go . parseExp Lit Add Mul
  





