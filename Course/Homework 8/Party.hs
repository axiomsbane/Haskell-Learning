module Party where

import Employee
import Data.Tree
import System.IO
import Data.List (sort)

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL empLis fun) = GL (emp:empLis) (fun + empFun emp)

-- Monoid instance of GuestList
instance Semigroup GuestList where 
  (<>) (GL lis1 fun1) (GL lis2 fun2) = GL (lis1 ++ lis2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gL1 gL2 = case compare gL1 gL2 of
  LT -> gL2
  EQ -> gL1
  GT -> gL1

treeFold :: Monoid b => (a -> b -> b) -> b -> Tree a -> b
treeFold f acc (Node root []) = f root acc
treeFold f acc (Node root subTree) = f root (mconcat (map (treeFold f acc) subTree))


-- fst -> with boss, snd -> without boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subList = (withBoss, withoutBoss)
  where withBoss = glCons boss (mconcat $ map snd subList)
        withoutBoss = mconcat $ map (\(l1, l2) -> moreFun l1 l2) subList

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun (fst pairu) (snd pairu)
  where pairu = maxFunCalc tree

maxFunCalc :: Tree Employee -> (GuestList, GuestList)
maxFunCalc (Node emp []) = (glCons emp mempty, mempty)
maxFunCalc (Node emp subTree) = nextLevel emp (map maxFunCalc subTree)


main = do 
  file <- readFile "company.txt"
  let empTree = read file
      guestList = maxFun empTree
      getFun (GL _ fun) = fun
      getEmps (GL emps _) = emps
      names = map empName (getEmps guestList)
  putStrLn ("Total fun: " ++ show (getFun guestList))
  mapM_ putStrLn (sort names)
  