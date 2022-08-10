{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char

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

----- Excercise 1 done 