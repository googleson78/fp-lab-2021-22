module ParserTasks where

import Parser
import Control.Applicative

-- Things we have available:
-- liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
-- parse a single char - nom :: Parser char
-- sequence parses - do-syntax
-- succeed without consuming input - pure :: a -> Parser a
-- try one of two - (<|>) :: Parser a -> Parser a -> Parser a
-- always fail - empty :: Parser a

-- parseList parseInt
-- "[1,2,3]" -> [1,2,3]
-- parseList nom
-- "[a,b,c]" -> ['a','b','c']

parseList :: Parser a -> Parser [a]
parseList px = do
  char '['
  xs <- many (px <* optional (char ','))
  char ']'
  pure xs

-- EXERCISE
-- Parse *only* the given char
-- EXAMPLES
-- >>> parse (char 'a') "a"
-- Just 'a'
-- >>> parse (char 'a') "b"
-- Nothing
-- >>> parse (char 'a') "ba"
-- Nothing
char :: Char -> Parser Char
char exp = do
  x <- nom
  if x == exp
  then pure x
  else empty

-- EXERCISE
-- Same as char, except instead of a specific char, we pass a
-- predicate that the char must satisfy
-- EXAMPLES
-- >>> parse (satisfy isDigit) "0"
-- Just '0'
-- >>> parse (satisfy isDigit) "a"
-- Nothing
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = undefined

-- EXERCISE
-- Parse a single digit. You'll need ord and fromIntegral here.
-- ord - return the ascii code(or unicode stuff) for a character
-- >>> ord 'a'
-- 97
-- >>> ord '0'
-- 48
-- >>> ord '9'
-- 57
-- >>> parse digitParser "0"
-- Just 0
-- >>> parse digitParser "20"
-- Just 2
-- >>> parse digitParser "a"
-- Nothing
digitParser :: Parser Integer
digitParser = undefined

-- EXERCISE
-- Convert a list of integers, assuming they are digits, to a number.
-- >>> digitListToInteger [1, 2, 3]
-- 123
-- >>> digitListToInteger [0, 1, 2, 3]
-- 123
digitListToInteger :: [Integer] -> Integer
digitListToInteger = undefined

-- EXERCISE
-- Combine digitParser and digitListToInteger to produce a parser for numbers.
-- EXAMPLES
-- >>> parse numberParser "asdf"
-- Nothing
-- >>> parse numberParser "0123"
-- Just 123
-- >>> parse numberParser "21303"
-- Just 21303
numberParser :: Parser Integer
numberParser = undefined

-- EXERCISE
-- Parse a lot of as seperated by bs
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy = undefined

-- EXERCISE
-- parse as many as possible from the first parser, then parse as many things with the second parser, as you did with the first
-- not really a^nb^n, but kind of
anbn :: Parser a -> Parser b -> Parser ([a], [b])
anbn n = undefined

-- EXERCISE
-- Parsing s-expressions (the syntax of lisps)
data SExp = SVar String | SList [SExp]
  deriving Show

-- Parse a variable name. Think about which things *should not* be a variable name.
-- EXAMPLES
-- >>> parse sVarParser "1"
-- Just (SVar "1")
-- >>> parse sVarParser "xyz"
-- Just (SVar "xyz")
-- >>> parse sVarParser "x y"
-- Just (SVar "x")
-- >>> parse sVarParser "(x y)"
-- Nothing
sVarParser :: Parser SExp
sVarParser = undefined

-- EXERCISE
-- Parse an SExpr. This comes before sListParser, which parses lists,
-- since they will need to be mutually recursive - an SList contains other SExprs,
-- after all. You can return to these examples after you've also implemented
-- sListParser.
-- EXAMPLES
-- >>> parse sExpParser "x"
-- Just (SVar "x")
-- >>> parse sExpParser "123"
-- Just (SVar "123")
-- >>> parse sExpParser "(1 2 3)"
-- Just (SList [SVar "1",SVar "2",SVar "3"])
-- >>> parse sExpParser "(1 (4 5))"
-- Just (SList [SVar "1",SList [SVar "4",SVar "5"]])
-- >>> parse sExpParser "((f 2 (3 4  ))   1 (4 5))"
-- Just (SList [SList [SVar "f",SVar "2",SList [SVar "3",SVar "4"]],SVar "1",SList [SVar "4",SVar "5"]])
sExpParser :: Parser SExp
sExpParser = undefined

-- EXERCISE
-- consume as many things that are spaces as possible
-- You can use isSpace to detect what a space is.
-- This parser should always succeed, since even 0 spaces are "as many as possible".
eatSpace :: Parser ()
eatSpace = undefined

-- EXERCISE
-- Parse a list of SExprs. As this description implies
-- this will need to be mutually recursive with the next function.
-- The easiest way to write this is t
-- EXAMPLES
-- >>> parse sListParser "(1 2 3)"
-- Just (SList [SVar "1",SVar "2",SVar "3"])
-- >>> parse sListParser "2"
-- Nothing
-- >>> parse sListParser "(1 2 3"
-- Nothing
-- >>> parse sListParser "1 2 3)"
-- Nothing
-- >>> parse sListParser "(2 3 (4 5))"
-- Just (SList [SVar "2",SVar "3",SList [SVar "4",SVar "5"]])
sListParser :: Parser SExp
sListParser = undefined
