{-# LANGUAGE InstanceSigs #-}

module ParserCombinators where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.Char (ord, isSpace)


-- what's a parser?
-- figure it out together
-- end goal: mostly ignore string manipulation, work with the Parser abstraction
-- and *combinators* (functions manipulating Parsers) instead

--type Parser a
-- parse :: ...
-- parser for char
-- nom
-- succeed

-- data LetterType = Vowel | Consonant
-- vowel or consonant
-- it's a functor - make it one, fmap, vowel or consonant
-- ask for record field, if it's not familiar just write a regular function

-- what if we want to parse two chars?
-- twoChars
-- this is fine, but it's not composable - can't run "parse two chars" twice in a row
-- can we do better? yes return the rest
-- twoChars'
-- twice
-- parse n chars
-- parse2
-- parse integer and then that many chars
-- bind, >>>=

-- in general, when we constructor more structured data from less structured data,
-- we need to be able to fail
-- what if our string is too short?
-- what if we want to parse an integer??
-- what if we want to parse a concrete letter/string
-- with failure
-- [] instead of maybe for multiple possible parses - I have an example if anyone is interested

-- this exists - Alternative
-- implement optional
-- and we get optional, many and some for free (although we could of course implement them ourselves)
-- some regular expressions as examples?

-- show do, explain later (next time) (? or just say a monad is something with bind)

-- mention runParser is helpful for debugging
-- show parse
{-
-}

{-
newtype Parser a = MkParser {runParser :: String -> [(String, a)]}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = MkParser $ fmap (fmap (fmap f)) $ runParser p

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = MkParser $ \str -> [(str, x)]
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px =
    MkParser $ \s ->
      let ress = runParser pf s
       in concatMap (\(rest, f) -> fmap (fmap f) $ runParser px rest) ress

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    MkParser $ \s ->
      concatMap (\(rest, x) -> runParser (f x) rest) $ runParser p s

instance MonadFail Parser where
  fail _ = empty

instance Alternative Parser where
  empty :: Parser a
  empty = MkParser $ \_ -> []
  px <|> py =
    MkParser $ \str ->
      runParser px str ++ runParser py str

nom :: Parser Char
nom = MkParser $ \x ->
  case x of
    [] -> []
    (c:cs) -> [(cs, c)]

endOfInput :: Parser ()
endOfInput =
  MkParser $ \str ->
    case str of
      [] -> [("", ())]
      _ -> empty

parse :: Parser a -> String -> Maybe a
parse p = fmap snd . listToMaybe . runParser p

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
char exp = undefined

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
-- parse as many form the first parser, then parse that many from the second parser
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
-}
