{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{- CIS 194 HW 10
   due Monday, 1 April
-} 

module AParser (Parser, runParser, satisfy, char, posInt) where

import           Control.Applicative
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Int
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first fn (x, y) = (fn x, y)

-- Exercise 1
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap fn parserA = Parser (fmap (first fn) . parseAFunc)
    where parseAFunc = runParser parserA

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\str -> Just (x, str))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser fnF) <*> (Parser fnA) = Parser createdFunc 
    where createdFunc str = case fnF str of
            Nothing          -> Nothing
            (Just (f, stur)) -> case fnA stur of
                                  Nothing           -> Nothing
                                  (Just (res, rem)) -> Just (f res, rem) 

fnn = (*2)

kuku :: Parser (a -> a)
kuku = pure id

pureStarV = pure id <*> posInt

pureVal :: Parser Int
pureVal = pure 2

homoLeft = pure fnn <*> pureVal

homoRight :: Parser Int
homoRight = pure (fnn 2)

-- type Name = String
-- data Employee = Emp { name :: Name, phone :: String }
--   deriving (Show)

-- parseName :: Parser Name
-- parseName = posInt (not . isDigit)

-- parsePhone :: Parser String
-- parsePhone = posInt isDigit

-- collecto = Emp <$> parseName <*> parsePhone

abParser :: Parser (Char, Char)
abParser = pure (\x y -> (x, y)) <*> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'b' <*> char 'a'

intPair :: Parser [Int]
intPair = (pure (\x y z -> [x, z])) <*> posInt <*>  (char ' ') <*> posInt

instance Alternative Parser where 
  empty :: Parser a 
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser createdFunc
    where createdFunc str = p1 str <|> p2 str


-------------------------------------------------------------------------
------------------Miscellaneous stuff below------------------------------
-------------------------------------------------------------------------

intOrUppercase :: Parser ()
intOrUppercase = (const ()) <$> ((show <$> posInt) <|> (show <$> satisfy isUpper))

(.+) :: (Applicative f, Num c) => f c -> f c -> f c
(.+) = liftA2 (+)
(.*) :: (Applicative f, Num c) => f c -> f c -> f c
(.*) = liftA2 (*)

-- names  = ["Joe", "Sara", "Mae"]
-- phones = ["555-5555", "123-456-7890", "555-4321"]

-- employees2 = getZipList $ Emp <$> ZipList names <*> ZipList phones


pair :: Applicative f => f a -> f b -> f (a,b)
pair fa fb = (\x y -> (x,y)) <$> fa <*> fb


replicateA :: Functor f => Int -> f a -> f [a]
replicateA rep = fmap (replicate rep)

sequenceA1  :: Applicative f => [f a] -> f [a]
sequenceA1 = foldr1 (liftA2 (++)) . makeItArr

makeItArr :: Functor f => [f a] -> [f [a]]
makeItArr = map (fmap (replicate 1)) 

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA fn = sequenceA1 . map fn

-------------------------------------------------------------------------
------------------Miscellaneous stuff above------------------------------
-------------------------------------------------------------------------








