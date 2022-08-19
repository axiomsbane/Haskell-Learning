-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then div x 2 else 3*x+1)

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node height _ _ _) = height

insert :: a -> Tree a -> Tree a
insert val Leaf = Node 0 Leaf val Leaf
insert val (Node height lTree curVal rTree)
  | getHeight lTree <= getHeight rTree = let newLTree = insert val lTree in Node (1 + max (getHeight newLTree) (getHeight rTree)) newLTree curVal rTree
  | otherwise = let newRTree = insert val rTree in Node (1 + max (getHeight lTree) (getHeight newRTree)) lTree curVal newRTree

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor ::  [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not (x && y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) [] 