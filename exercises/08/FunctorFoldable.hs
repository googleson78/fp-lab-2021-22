-- allows us to write signatures on instance implementations
{-# LANGUAGE InstanceSigs #-}
-- allows ghc to generate instances of Functor
{-# LANGUAGE DeriveFunctor #-}
-- allows ghc to generate instances of Foldable
{-# LANGUAGE DeriveFoldable #-}
-- stop using * to mean Type
{-# LANGUAGE NoStarIsType #-}
-- allows us to write kind signatures
{-# LANGUAGE KindSignatures #-}
-- needed for Cont
-- you can read the explanation where we implemented Nats via lambdas
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

module FunctorFoldable where

import Prelude hiding (Foldable(..), Functor(..), length, sum, toList)
import Data.Monoid (Endo(..), Sum (Sum, getSum))
import Data.Kind (Type)

-- motivation:

-- map, foldMap - same funs for List, Maybe, Tree

-- map :: (a -> b) -> [] a -> [] b
-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- mapTree :: (a -> b) -> Tree a -> Tree b

-- class Eq a where
--   (==) :: a -> a -> a

-- class MyEq (a :: Type) where

-- Type Тип
-- *

-- Type -> Type
-- category theory
-- Functor

-- fmap id xs == xs
-- fmap f (fmap g xs) == fmap (f . g) xs
-- fmap f . fmap g == fmap (f . g)
--class Functor (f :: Type -> Type) where
--  fmap :: (a -> b) -> f a -> f b
--
--instance Functor [] where
--  fmap :: (a -> b) -> [a] -> [b]
--  fmap = map
--
--instance Functor Maybe where
--  fmap :: (a -> b) -> Maybe a -> Maybe b
--  fmap _ Nothing = Nothing
--  fmap f (Just x) = Just (f x)

-- kind
-- instance Mappable Int where
--   fmap :: (a -> b) -> Int a -> Int b

-- стойност :: тип :: вид
-- value :: type :: kind

-- 0        1       2

-- 0 :: Int :: *
-- Int :: Type
-- Maybe :: Type -> Type

type Apply f a = f a

--x :: Apply Maybe Int
---- x :: Maybe Int
--x = Just 5

-- f :: (Int -> Int) -> Int -> Int
-- f = undefined


-- abstract away
-- * name overloading for commonly used functions
-- * can write lots of operations that're polymorphic over the container
--    like in the homework

-- show functor
-- what things can be instances of it?
-- [] is fine. Int isn't. Why?
-- Explain kinds. :kind. What does "Type" mean? What does "*" mean?
-- NoStarIsKind

type Id a = a

newtype Identity a = MkIdentity a
  deriving Show

data Pair a = MkPair a a

data Tuple a b = MkTuple a b
  deriving Show

--instance Functor [] where
--  fmap = undefined
--
--instance Functor Maybe where
--  fmap = undefined
--
--instance Functor Tree where
--  fmap = undefined

--listFoldMap :: Monoid m => (a -> m) -> [a] -> m
--listFoldMap _ [] = mempty
--listFoldMap f (x:xs) = f x <> listFoldMap f xs
--
--maybeFoldMap :: Monoid m => (a -> m) -> Maybe a -> m
--maybeFoldMap _ Nothing = mempty
--maybeFoldMap f (Just x) = f x
--
--data Tree a = Empty | Node (Tree a) a (Tree a)
--
--treeFoldMap :: Monoid m => (a -> m) -> Tree a -> m
--treeFoldMap _ Empty = mempty
--treeFoldMap f (Node l x r) =
--  treeFoldMap f l <> f x <> treeFoldMap f r
--
--class Foldable f where
--  foldMap :: Monoid m => (a -> m) -> f a -> m
--  foldr :: (a -> b -> b) -> b -> f a -> b
--
--instance Foldable [] where
--  foldr _ v [] = v
--  foldr f v (x:xs) = f x $ foldr f v xs
--  foldMap = undefined
--
--sum :: Foldable f => f Int -> Int
--sum = foldr (+) 0
--
--toList :: Foldable f => f a -> [a]
--toList = undefined

-- foldr == foldMap

-- foldMap' f = foldr (\x r -> f x <> r) mempty
-- foldr' = ...foldMap...

-- length, sum, all, any, elem, find

-- Laws - what are they useful for? transformations
-- fmap id == id
-- fmap f . fmap g == fmap (f . g)

-- show Foldable
-- mention that it's similar to having a read-only Iterator-like thing
-- mention foldMap and foldr being equivalent

--
-- Show that they can be derived and it's very useful
--data Tree a
--  = Empty
--  | Node (Tree a) a (Tree a)
--  deriving (Show, Eq, Ord, Functor, Foldable)

-- laws:
-- fmap id == id
-- fmap f . fmap g == id
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 4 <$>

class Foldable (f :: Type -> Type) where
  foldr :: (a -> b -> b) -> b -> f a -> b
  foldr f v xs = appEndo (foldMap (Endo . f) xs) v

  foldMap :: Monoid m => (a -> m) -> f a -> m
  foldMap f xs = foldr ((<>) . f) mempty xs

-- Some resources:
-- https://wiki.haskell.org/Typeclassopedia
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Foldable.html#g:7

-- EXERCISE
-- The Identity newtype is a functor in a very "trivial" way,
-- since it's only holding a value.
instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap = undefined

-- EXERCISE
-- Pairs of two elements of the same type similarly can be a Functor.
instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap = undefined

-- EXERCISE
instance Foldable Identity where

-- EXERCISE
instance Foldable Pair where

-- EXERCISE
-- Calculate the length of a foldable
length :: Foldable f => f a -> Int
length = undefined

-- EXERCISE
-- Calculate sum of a foldable
sum :: (Foldable f, Num a) => f a -> a
sum = undefined

-- EXERCISE
-- Convert a foldable to a list
toList :: Foldable f => f a -> [a]
toList = undefined

-- EXERCISE
-- Reminder:
-- data Either a b
--   = Left a
--   | Right b
-- Either can be a functor over each of its arguments.
-- It is, however, traditionally made a functor on its right argument,
-- because that's easier syntactically, and also because it's right argument
-- "contains" a result value, which you usually want to map over.

-- EXAMPLES
-- >>> fmap succ $ Left 13
-- >>> fmap succ $ Right 13
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap = undefined

-- EXERCISE
-- Builtin tuples are similarly only a functor over their second argument.
instance Functor ((,) w) where
  fmap :: (a -> b) -> (w, a) -> (w, b)
  fmap = undefined

-- EXERCISE
-- tuples and Either are also Foldable over only their second argument
-- leading to "weird" behaviour like
-- > length ('a', 'b')
-- 1
-- > length $ Left 3
-- 0
-- > length $ Right 3
-- 1
-- > elem 3 (3,4)
-- False
-- > elem 3 (4,3)
-- True
instance Foldable (Either e) where

instance Foldable ((,) w) where

-- A "rose tree" or simply a tree in graph theory
-- It has arbitrary branching at each level, represented by a list of children.
data Rose a = MkRose a [Rose a]
  deriving Show

-- Implement Functor for it.
-- EXAMPLES
-- >>> fmap succ (MkRose 5 [MkRose 6 [], MkRose 7 [], MkRose 8 [MkRose 9 []]])
-- MkRose 6 [MkRose 7 [],MkRose 8 [],MkRose 9 [MkRose 10 []]]
instance Functor Rose where
  fmap :: (a -> b) -> Rose a -> Rose b
  fmap = undefined

-- Implement Foldable for it.
instance Foldable Rose where

-- EXERCISE
-- We can only keep "the shape" of something, and discard it's contents,
-- replacing them with a pure value.
(<$) :: Functor f => a -> f b -> f a
(<$) = undefined

-- EXERCISE
-- Useful in situations that we are yet to encounter.
void :: Functor f => f a -> f ()
void = undefined

-- EXERCISE
-- When you fix the type of the left argument, functions are a functor.
-- ideally we would write
-- instance Functor (r ->) where
-- but that isn't syntactically valid Haskell.
-- Follow the types and use type holes!
-- (putting _ to make the compiler tell you what type is expected)
instance Functor ((->) r) where
  fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap = undefined

-- EXERCISE
-- What if we want to be able to map over both the types in (,) and Either?
-- This is traditionally called a "Bifunctor" with a bimap method.
-- It's available in base under the same name. (along wit the functions below)
-- Think about what class method it should have, and then implement instances
-- for (,) and Either
-- We've specified that the f is going to be a type function of *two* arguments here.
class Bifunctor (f :: Type -> Type -> Type) where

-- EXERCISE
instance Bifunctor (,) where

-- EXERCISE
instance Bifunctor Either where

-- EXERCISE
-- With a Bifunctor, you should be able to map over only either of the arguments:
first :: Bifunctor f => (a -> c) -> f a b -> f c b
first = undefined

second :: Bifunctor f => (b -> c) -> f a b -> f a c
second = undefined

-- EXERCISE
-- You should also be able to map over both of them at the same time with a single function.
both :: Bifunctor f => (a -> b) -> f a a -> f b b
both = undefined

-- EXERCISE
-- "The continuation monad"
-- When we have a Monad for this, we can use it to "escape callback hell"
-- more info:
-- this seems decent - https://www.haskellforall.com/2012/12/the-continuation-monad.html
-- or the chapter in Thinking with Types dedicated to this topic
newtype Cont a = Cont {runCont :: forall r. (a -> r) -> r}

toId :: Cont a -> a
toId = undefined

fromId :: a -> Cont a
fromId = undefined

-- Can you do this without using toId and fromId?
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap = undefined
