module Calc where
import ExprT
import Parser (parseExp)


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
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)


-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + (-6)"
-- testInteger = testExp :: Maybe Integer
-- testBool = testExp :: Maybe Bool
-- testMM = testExp :: Maybe MinMax
-- testSat = testExp :: Maybe Mod7