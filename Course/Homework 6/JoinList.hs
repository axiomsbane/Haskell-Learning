{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Editor (runEditor, editor)

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty joinListB = joinListB
(+++) joinListA Empty = joinListA
(+++) joinListA joinListB = Append (tag joinListA <> tag joinListB) joinListA joinListB 


cacheSizeTree :: Monoid a1 => JoinList a1 a2 -> JoinList (a1, Size) a2
cacheSizeTree Empty = Empty
cacheSizeTree (Single m a) = Single (m , Size 1) a
cacheSizeTree (Append _ joinList1 joinList2) = cacheSizeTree joinList1 +++ cacheSizeTree joinList2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ val tree = indexJHelp val (cacheSizeTree tree)

indexJHelp :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJHelp _ Empty = Nothing
indexJHelp idx joinList
  | idx < 0 = Nothing
  | idx >= getSize (size $ tag joinList) = Nothing
indexJHelp idx (Single b a) = Just a
indexJHelp idx (Append val joinList1 joinList2)  
  | (idx + 1) <= getSize szLeft = indexJHelp idx joinList1
  | otherwise = indexJHelp (idx - getSize szLeft) joinList2
    where szLeft = size $ tag joinList1  
          szRight = size $ tag joinList2


(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)


jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

newtype Product = Product Int
  deriving (Eq, Ord, Num, Show)

instance Semigroup Product where
  (<>) = (*)

instance Monoid Product where
  mempty = Product 1 

getProduct :: Product -> Int
getProduct (Product a) = a
jls = Append (Product 210) (Append (Product 30) (Single (Product 5) 'y') (Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a'))) (Single (Product 7) 'h')

instance Sized Product where
  size = Size . getProduct

checker :: Int -> Bool
checker i = (indexJ i jls) == (jlToList jls !!? i)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList (b, Size) a
dropJ val joinList = dropJHelp val (cacheSizeTree joinList)


dropJHelp :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJHelp sz Empty = Empty
dropJHelp sz joinList
  | sz <= 0 = joinList
  | sz >= getSize (size $ tag joinList) = Empty
dropJHelp sz single@(Single m a) 
  | sz >= getSize (size m) = Empty
  | otherwise = single
dropJHelp sz (Append m joinList1 joinList2)
  | sz < szCur = (dropJHelp minChoose joinList1) +++ dropJHelp (sz - minChoose) joinList2
  | otherwise = Empty   
  where
    szLeft = getSize $ size $ tag joinList1
    szCur = getSize $ size m
    minChoose = min sz szLeft


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList (b, Size) a
takeJ val tree = takeJHelp val (cacheSizeTree tree) 


takeJHelp :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJHelp sz Empty = Empty
takeJHelp sz joinList
  | sz <= 0 = Empty
  | sz >= getSize (size $ tag joinList) = joinList
takeJHelp sz single@(Single m a) 
  | sz >= getSize (size m) = single
  | otherwise = Empty
takeJHelp sz appu@(Append m joinList1 joinList2)
  | sz < szCur = (takeJHelp minChoose joinList1) +++ takeJHelp (sz - minChoose) joinList2
  | otherwise = appu   
  where
    szLeft = getSize $ size $ tag joinList1
    szCur = getSize $ size m
    minChoose = min sz szLeft


scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

generateSingles :: [String] -> [JoinList (Score, Size) String]
generateSingles = map (\x -> Single (scoreString x, Size 1) x)

instance Semigroup (JoinList (Score, Size) String) where
  (<>) = (+++)
instance Monoid (JoinList (Score, Size) String) where
  mempty = Empty

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ str) = str
  toString (Append _ j1 j2) = toString j1 ++ toString j2
  --fromString str = Single (scoreString str, 1) str
  -- fromString []     = Empty
  -- fromString (x:[]) = Single (score x, 1) (x:[])
  -- fromString str    = Single (scoreString str, 1) str
  fromString str = mconcat $ generateSingles (lines str)
  line idx joinList = indexJHelp idx joinList
  replaceLine n str joinList = takeJHelp (n) joinList +++ fromString  str +++ dropJHelp (n+1) joinList
  numLines joinList = getSize $ size $ tag joinList
  value = getScore . fst . tag 


main = runEditor editor (Single (Score 0, Size 0) "" :: JoinList (Score, Size) String)

  

