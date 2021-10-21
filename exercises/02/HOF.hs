{-# LANGUAGE RankNTypes #-}
module HOF where

import Prelude hiding (const, ($), curry, uncurry, id, swap)

data Nat
  = Zero
  | Suc Nat
  deriving Show


-- f :: Integer -> Integer -> Integer
f :: Integer -> (Integer -> Integer)
-- > f x y
-- > (f x) y
-- > (f x) y
f x y = x + y
-- f x y = x * 10

-- >>> (\<args> -> <body>) <args>
-- <body>

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Suc (integerToNat (n - 1))

eqNat :: Nat -> Nat -> Bool
eqNat Zero Zero = True
eqNat Zero (Suc _) = False
eqNat (Suc _) Zero = False
eqNat (Suc n) (Suc m) = eqNat n m

-- TODO live
-- lambdas also
-- identity
id :: a -> a
id x = x

addOne :: Integer -> Integer
addOne = \n -> 1 + n

-- TODO: live, example Nats
-- show how it's the same as id?
apply :: (a -> b) -> a -> b
apply f x = f x
-- > Suc (Suc (Suc (.........)))

-- Suc $ Suc $ Suc $ Suc Zero
-- Suc $ (Suc $ (Suc $ (Suc Zero)))
-- Suc (Suc $ (Suc $ (Suc Zero)))
-- Suc (Suc (Suc (Suc Zero)))

($) :: (a -> b) -> a -> b
($) = apply

infixr 0 $

($$$) :: Integer -> Integer -> Integer -> Integer
($$$) x y z = x + y + z

-- TODO live
addTwo :: Nat -> Nat
addTwo = addNat $ Suc $ Suc Zero

-- TODO live
data Tuple a b = MkTuple a b
  deriving Show

-- (a, b)

x :: Tuple Integer Char
x = MkTuple 42 'a'

-- https://jitsi.ludost.net/fp

fstTuple :: Tuple a b -> a
fstTuple (MkTuple x _) = x
-- TODO mention (a, b)
-- TODO sections

addNat :: Nat -> (Nat -> Nat)
addNat Zero m = m
addNat (Suc n) m = Suc $ addNat n m

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Suc n) = 1 + natToInteger n

-- EXERCISE
-- Take two arguments and return the second.
-- This is called const because if we think of it as a function
-- on one argument x, it returns a function that when called, always returns x
-- It is also practically always used partially applied.
-- EXAMPLES
-- >>> const 5 6
-- 5
-- >>> applyTwice (const 42) 1337
-- 42
const :: a -> b -> a
const = undefined

-- EXERCISE
-- Compose two functions, very useful very often
-- there's a builtin (.) for this - the dot mimics mathematical notation f o g
-- EXAMPLES
-- >>> let f = compose (+3) (*5) in f 4
-- 23
-- >>> let f = compose (*5) (+5) in f 4
-- 45
compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

-- EXERCISE
-- Iterate a function f n times over a base value x.
-- EXAMPLES
-- >>> iterateN (+1) 1 10
-- 11
-- >>> iterateN (*2) 1 10
-- 1024
iterateN :: (a -> a) -> a -> Integer -> a
iterateN = undefined

-- EXERCISE
-- Swap the two elements of a tuple
-- EXAMPLES
-- >>> swap $ MkTuple 42 69
-- MkTuple 69 42
swap :: Tuple a b -> Tuple b a
swap = undefined

-- EXERCISE
-- Apply a function to only the first component of a tuple
-- EXAMPLES
-- >>> first (*2) $ MkTuple 21 1337
-- MkTuple 42 1337
first :: (a -> b) -> Tuple a c -> Tuple b c
first = undefined

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> curryTuple (\(MkTuple x y) -> x * y) 23 3
-- 69
curry :: (Tuple a b -> c) -> a -> b -> c
curry = undefined

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> uncurryTuple (\x y -> x + y) $ MkTuple 23 46
-- 69
uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry = undefined

-- EXERCISE
-- Apply two different functions to the two different arguments of a tuple
-- Think about what the type should be.
-- mapTuple :: ???
-- mapTuple = undefined

-- EXERCISE
-- Find the fixpoint (f x == x) of a function, if it exists.
-- Assume your input is a natural number!
-- Use a local helper function to help you "iterate" over the results of f
-- EXAMPLES
-- >>> fixpoint fact
-- 1
-- >>> fixpoint $ \x -> if x < 68 then x + 1 else 69
-- 69
-- >>> fixpoint (const 42)
-- 42
-- >>> fixpoint (+1)
-- <don't actually execute this in your editor! - loops forever since the given function dosn't have a fixed point>
fixpoint :: (Integer -> Integer) -> Integer
fixpoint = undefined

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
