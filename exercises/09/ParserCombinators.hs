{-# LANGUAGE InstanceSigs #-}
module ParserCombinators where
import Control.Applicative
import Data.Maybe (listToMaybe)

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

char :: Char -> Parser Char
char exp = do
  x <- nom
  if x == exp
  then pure x
  else empty

parse :: Parser a -> String -> Maybe a
parse p = fmap snd . listToMaybe . runParser p
