-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0 = []
  | otherwise = getOnes n : toDigitsRev ((n - getOnes n) `div` 10)

getOnes :: Integer -> Integer
getOnes n = mod n 10

toDigits :: Integer -> [Integer]
toDigits n = reverser (toDigitsRev n)

reverser :: [a] -> [a]
reverser [] = []
reverser (x:xs) = reverser xs ++ [x]


--Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lis = go 1 lis (length lis)
  where 
    go val [] len = []
    go val (x:xs) len
      | mod val 2 /= mod len 2 = 2 * x : go (val + 1) xs len
      | otherwise = x : go (val + 1) xs len

--Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

--Exercise 4
validate :: Integer -> Bool
validate num = 0 == sumDigits (doubleEveryOther (toDigits num)) `mod` 10