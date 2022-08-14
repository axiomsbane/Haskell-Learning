module Golf where
import Data.List

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

---Exercise 1

skips :: [a] -> [[a]]
skips lis = go lis 1 (length lis) (zip [1..(length lis)] lis)
  where 
    go lis idx len pairList
      | idx > length lis = []
      | otherwise = map snd (filter (\(a, b) -> mod a idx == 0) pairList) : go lis (1 + idx) len pairList

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

---Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) =  [y | x < y && y > z] ++ localMaxima (y:z:zs)
localMaxima _ = []

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

---Exercise 3

constStr :: String
constStr = "\n==========\n0123456789\n"

mapToCounts :: [Int] -> [Int]
mapToCounts lis = map (\x -> length $ filter (== x) lis) [0..9]

getEveryLevel :: Int -> Int -> [Int] -> [String]
getEveryLevel idx maxCnt cnts 
  | idx > maxCnt = []
  | otherwise = map (\x -> if idx <= x then '*' else ' ') cnts : getEveryLevel (idx + 1) maxCnt cnts

histogram :: [Int] -> String
histogram lis = intercalate "\n" (reverse $ getEveryLevel 1 (maximum ctrList) ctrList) ++ constStr
  where 
    ctrList = mapToCounts lis