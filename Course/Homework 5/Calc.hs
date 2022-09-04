{-# LANGUAGE FlexibleInstances #-}

module Calc where
import ExprT
import Parser (parseExp)
import qualified Data.Map as M
import Data.Maybe

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
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
  
class Expr a where
  mul :: a -> a -> a
  add :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  mul = Mul
  add = Add
  lit = Lit

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  mul x y = x * y
  add x y = x + y
  lit = id

instance Expr Bool where
  mul x y = x && y
  add x y = x || y
  lit x 
    | x <= 0 = False
    | otherwise = True

instance Expr MinMax where
  lit x = MinMax x
  mul (MinMax x) (MinMax y) = MinMax $ min x y
  add (MinMax x) (MinMax y) = MinMax $ max x y

instance Expr Mod7 where
  lit x = Mod7 $ mod x 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7
  add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7


-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + (-6)"
-- testInteger = testExp :: Maybe Integer
-- testBool = testExp :: Maybe Bool
-- testMM = testExp :: Maybe MinMax
-- testSat = testExp :: Maybe Mod7

{-
The below is to create a calculator with
the feature to store expressions with variables
VarExprT will be an instance of both Expr & HasVars
-}

class HasVars a where
  var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var str = \mMap -> M.lookup str mMap

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \mMap -> Just x
  add f1 f2 = \mMap -> if isNothing (f1 mMap) || isNothing (f2 mMap)
                        then 
                          Nothing
                        else
                          Just (fromJust (f1 mMap) + fromJust (f2 mMap))
  mul f1 f2 = \mMap -> if isNothing (f1 mMap) || isNothing (f2 mMap)
                        then 
                          Nothing
                        else
                          Just (fromJust (f1 mMap) * fromJust (f2 mMap))

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer)-> Maybe Integer
withVars vs exp = exp $ M.fromList vs
