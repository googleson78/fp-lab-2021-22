{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.Char (ord, isSpace, isUpper)


-- agenda:
-- 0. reminder of what we did last time - look at the combinators and show them briefly again
-- 1. newtype -- ask if fine with records?
-- 2. lettertype example + optional as example motivators for fmap? -- add Functor
-- 3. add Functor, add Applicative
-- 4. add Monad, show do desugaring
-- 5. mention how we get some free stuff - many and some are important, when, guard, sequence, traverse?
-- 6. write simple stack calculator parser to show how we use a parser
--    add
--    mult
--    incr
--    push
-- 7. json?

newtype Parser a = MkParser (String -> [(String, a)])

runParser :: Parser a -> String -> [(String, a)]
runParser (MkParser px) str = px str

parse :: Parser a -> String -> Maybe a
parse px str =
  case runParser px str of
    [] -> Nothing
    (_, x):_ -> Just x

nom :: Parser Char
nom =
  MkParser $ \str ->
    case str of
      [] -> []
      c:rest -> [(rest, c)]

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
px >>>= f =
  MkParser $ \str ->
    [(rest2, y)
      | (rest1, x) <- runParser px str,
        (rest2, y) <- runParser (f x) rest1]

parseThreeChars :: Parser (Char, Char, Char)
parseThreeChars =
  --nom >>>= (\x -> (nom >>>= \y -> (nom >>>= \z -> succeed (x, y, z))))
  nom >>>= \x ->
  nom >>>= \y ->
  nom >>>= \z ->
    succeed (x, y, z)

upperLetter :: Parser Char
upperLetter =
  nom >>>= \x ->
    if isUpper x
    then succeed x
    else failParser

data LetterType = Vowel | Consonant
  deriving Show

classifyLetter :: Char -> LetterType
classifyLetter x =
  if x `elem` "aeuiow"
  then Vowel
  else Consonant

-- взима буквичка от входа
-- ако е гласна връща Vowel, в противен случай Consonant
letterTypeParser :: Parser LetterType
letterTypeParser =
  fmap classifyLetter nom

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px =
    MkParser $ \str ->
      map (\(str, a) -> (str, f a)) $ runParser px str

succeed :: a -> Parser a
succeed x =
  MkParser $ \str -> [(str, x)]

parse2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parse2 f px py =
  MkParser $ \str ->
    [(rest2, f x y)
      | (rest1, x) <- runParser px str,
        (rest2, y) <- runParser py rest1]

instance Applicative Parser where
  pure :: a -> Parser a
  pure = succeed

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 = parse2

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = (>>>=)

parseThreeChars' :: Parser (Char, Char, Char)
parseThreeChars' = do
  x <- nom
  y <- nom
  z <- nom
  pure (x, y, z)

--  nom >>>= \x ->
--  nom >>>= \y ->
--  nom >>>= \z ->
--    succeed (x, y, z)

upperLetter' :: Parser Char
upperLetter' = do
  x <- nom

  let checked = isUpper x

  if checked
  then pure x
  else failParser

-- ако fail успее ще ни даде a
failParser :: Parser a
failParser = MkParser $ \_ -> []

-- px <|> py
-- str
-- пробвай px върху str, ако стане, супер
-- ако не, пробвай py върху str

instance Alternative Parser where
  empty :: Parser a
  empty = failParser

  (<|>) :: Parser a -> Parser a -> Parser a
  px <|> py =
    MkParser $ \str -> runParser px str ++ runParser py str

optional' :: Parser a -> Parser (Maybe a)
optional' px = fmap Just px <|> pure Nothing

onlyUppers :: Parser [Char]
onlyUppers = do
  x <- optional' upperLetter
  case x of
    Nothing -> pure []
    Just x' -> do
      xs <- onlyUppers
      pure $ x' : xs


-- import Control.Applicative
-- class Alternative f where
--   empty :: f a - "винаги провал"
--   (<|>) :: f a -> f a -> f a

-- f = do
--   x <- y
--   let z = u
--   <expr>

-- syntax sugar for

-- f =
--   y >>= \x -> let z = u in <expr>

--  nom >>>= \x ->
--    if isUpper x
--    then succeed x
--    else failParser

-- sequence :: Applicative m => [m a] -> m [a]
-- sequence :: [Parser a] -> Parser [a]
--

-- instance Monad [] where
-- instance Monad Maybe where

-- class Applicative f where
--   pure :: a -> f a
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- lift2Maybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- lift2List :: (a -> b -> c) -> [a] -> [b] -> [c]

-- (>>>=) :: Parser a -> (a -> Parser b) -> Parser b
-- parse2 = ...>>>=...
-- fmap :: (a -> b) -> Parser a -> Parser b
