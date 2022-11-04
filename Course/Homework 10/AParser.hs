{-# LANGUAGE InstanceSigs #-}

{- CIS 194 HW 10
   due Monday, 1 April
-} 

module AParser where

import           Control.Applicative
import Test.QuickCheck
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
posInt :: (Char -> Bool) -> Parser String
posInt fn = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (ns, rest)
      where (ns, rest) = span fn xs

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
  pure x = Parser (\_ -> Just (x, ""))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser fnF) (Parser fnA) = Parser createdFunc 
    where createdFunc = \str -> case (fnA str) of
                                  (Nothing)  -> Nothing
                                  (Just res) -> case (fnF (snd res)) of
                                                  (Nothing)  -> Nothing
                                                  (Just fin) -> Just ((fst fin) (fst res), snd fin)
  
fnn = (*2)

kuku :: Parser (a -> a)
kuku = pure id

pureStarV = pure id <*> posInt

-- idd str = ((runParser pureStarV) str) == ((runParser posInt) str)

-- actualFunc :: String -> Maybe (Int -> Int, String)
-- actualFunc _ = Just (fnn, "")

pureVal :: Parser Int
pureVal = pure 2

homoLeft = pure fnn <*> pureVal

homoRight :: Parser Int
homoRight = pure (fnn 2)

homoMorphism str = ((runParser homoLeft) str) == ((runParser homoRight) str)

-- lefFmap = fmap (2*) posInt 
-- rigFmap = pure (2*) <*> posInt

-- checkFmap str = ((runParser lefFmap) str) == ((runParser rigFmap) str)



type Name = String
data Employee = Emp { name :: Name, phone :: String }
  deriving (Show)

parseName :: Parser Name
parseName = posInt (not . isDigit)

parsePhone :: Parser String
parsePhone = posInt isDigit

collecto = Emp <$> parseName <*> parsePhone