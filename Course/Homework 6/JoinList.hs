{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JoinList where

import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _)  = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
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