{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char

-- Excercise 1 done 
splitBySpace :: String -> String -> [String]
splitBySpace [] acc = [reverse acc]
splitBySpace (x:xs) acc
  | x == ' ' = reverse acc : splitBySpace xs []
  | otherwise = splitBySpace xs (x:acc)

parseMessage :: String -> LogMessage
parseMessage lis = parseMessageUtil (splitBySpace lis [])
  
isNumberr :: [Char] -> Bool
isNumberr str = foldr (&&) True (map isDigit str)

toNum :: String -> Int
toNum numm = read numm :: Int

getInfoOrWarn :: String -> String -> [String] -> LogMessage 
getInfoOrWarn logLevel timeStamp str 
  | logLevel == "I" = LogMessage Info (toNum timeStamp) (unwords str)
  | logLevel == "W" = LogMessage Warning (toNum timeStamp) (unwords str)
  | otherwise = Unknown (unwords str)

parseMessageUtil :: [String] -> LogMessage
parseMessageUtil [] = Unknown []
parseMessageUtil [x] = Unknown x
parseMessageUtil [x, y] = Unknown (x++y)
parseMessageUtil [x, y, z]
  | (x == "I" || x == "W") && isNumberr y = getInfoOrWarn x y [z]
  | otherwise = Unknown (unwords [x, y, z])
parseMessageUtil (x:y:z:zs) 
  | (x == "I" || x == "W") && isNumberr y = getInfoOrWarn x y (z:zs)
  | (x == "E") && isNumberr y && isNumberr z && (toNum y >= 1 && toNum y <= 100) = LogMessage (Error (toNum y)) (toNum z) (unwords zs)
  | otherwise = Unknown (unwords (x:y:z:zs))

parse :: String -> [LogMessage]
parse txt = map parseMessage (lines txt)

--Exercise 2
getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ tmpStmp _) = tmpStmp
getTimeStamp (Unknown _) = 0

insert :: LogMessage -> MessageTree -> MessageTree 
insert (Unknown _) mTree = mTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg (Node mTreeLeft mLogMsg mTreeRight)
  | getTimeStamp logMsg >= getTimeStamp mLogMsg = Node mTreeLeft mLogMsg (insert logMsg mTreeRight)
  | otherwise = Node (insert logMsg mTreeLeft) mLogMsg mTreeRight


--Exercise 3 
build :: [LogMessage] -> MessageTree
build = acc Leaf
  where 
    acc curTree [] = curTree
    acc curTree (x:xs) = acc (insert x curTree) xs

-- Excercise 4 inOrder traversal
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree logMsg rTree) = concat [inOrder lTree, [logMsg], inOrder rTree]

-- Exercise 5 : need to get all the log messages that have severity level >= 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong  = whatWentWrongUtil . inOrder . build 

isErrorMoreThan50 :: LogMessage -> Bool
isErrorMoreThan50  (LogMessage (Error level) _ _) = level >= 50
isErrorMoreThan50 (LogMessage _ _ _) = False
isErrorMoreThan50 (Unknown _) = False

getStrMsg :: LogMessage -> String
getStrMsg (LogMessage _ _ str) = str
getStrMsg (Unknown str) = str

whatWentWrongUtil :: [LogMessage] -> [String]
whatWentWrongUtil [] = []
whatWentWrongUtil (x:xs)
  | isErrorMoreThan50 x = getStrMsg x : whatWentWrongUtil xs
  | otherwise = whatWentWrongUtil xs