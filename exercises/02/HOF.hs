{-# LANGUAGE RankNTypes #-}
module HOF where

import Prelude hiding (const, ($), curry, uncurry, id, swap)

-- TODO mention homework!

data Nat
  = Zero
  | Suc Nat
  deriving Show

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Suc (integerToNat (n - 1))

eqNat :: Nat -> Nat -> Bool
eqNat Zero Zero = True
eqNat Zero (Suc _) = False
eqNat (Suc _) Zero = False
eqNat (Suc n) (Suc m) = eqNat n m

-- TODO live
data Tuple

--fstTuple :: Tuple a b -> a
-- TODO mention (a, b)

-- TODO live
-- lambdas also
id :: a -> a
id = undefined

-- TODO: live, example Nats
-- show how it's the same as id?
apply :: (a -> b) -> a -> b
apply = undefined

($) :: (a -> b) -> a -> b
($) = apply

infixr 0 $

-- TODO live
addTwo :: Nat -> Nat
addTwo = undefined
-- TODO sections

natToInteger :: Nat -> Integer
natToInteger = undefined

addNat :: Nat -> Nat -> Nat
addNat = undefined

-- TODO live if time permits, addNat, natToInteger
foldNat :: (a -> a) -> a -> Nat -> a
foldNat = undefined

-- EXERCISE
-- Take two arguments and return the second.
-- This is called const because if we think of it as a function
-- on one argument x, it returns a function that when called, always returns x
-- It is also practically always used partially applied.
-- EXAMPLES
-- >>> const 5 6
-- 5
-- >>> foldNat (const 42) 1337 Zero
-- 42
-- >>> foldNat (const 42) 1337 $ Suc Zero
-- 42
const :: a -> b -> a
const = undefined

-- EXERCISE
-- Compose two functions, very useful very often
-- there's a builtin (.) for this - the dot mimics mathematical notation f o g
-- EXAMPLES
-- >>> let f = (+3) . (*5) in f 4
-- 23
-- >>> let f = (*5) . (+5) in f 4
-- 45
compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

-- EXERCISE
-- Implement multNat using foldNat.
-- EXAMPLES
-- >>> natToInteger $ multNat (integerToNat 2) (integerToNat 3)
-- 6
multNat :: Nat -> Nat -> Nat
multNat = undefined

-- EXERCISE
-- Implement exponentiation(n ^ m) using foldNat.
-- EXAMPLES
-- >>> natToInteger $ expNat (integerToNat 2) (integerToNat 10)
-- 1024
expNat :: Nat -> Nat -> Nat
expNat = undefined

-- EXERCISE
-- Iterate a function f n times over a base value x.
-- Wait, this looks familar. Didn't we do this already?
-- You can even use compose and eta reduction in some way, here.
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
-- swap :: Tuple a b -> Tuple b a
swap = undefined

-- EXERCISE
-- Apply a function to only the first component of a tuple
-- EXAMPLES
-- >>> first (*2) $ MkTuple 21 1337
-- MkTuple 42 1337
-- first :: (a -> b) -> Tuple a c -> Tuple b c
first = undefined

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> curryTuple (\(MkTuple x y) -> x * y) 23 3
-- 69
-- curry :: (Tuple a b -> c) -> a -> b -> c
curry = undefined

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> uncurryTuple (\x y -> x + y) $ MkTuple 23 46
-- 69
-- uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry = undefined

-- EXERCISE
-- Apply two different functions to the two different arguments of a tuple
-- Think about what the type should be.
-- mapTuple :: ???
-- mapTuple = undefined

-- EXERCISE
-- Look at the recursor for nats - foldNat. In there we replaced Nats constructors, with things.
-- Think about how a recursor for tuples should look like, and implement it.
-- foldTuple :: ???
-- foldTuple = undefined

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

-- EXERCISE
-- If Nats can be converted to "n times applications" via foldNat,
-- is it perhaps true that "n times applications" can also be converted to Nats somehow?
-- Ignore the forall, it just says the given function *must* be polymorphic.
-- EXAMPLES
-- >>> iterateToNat (\f x -> f (f (f x)))
-- Suc (Suc (Suc Zero))
iterateToNat :: (forall a. (a -> a) -> a -> a) -> Nat
iterateToNat f = undefined

-- EXERCISE
-- Figure out how to extend foldNat so you can easily write factorial over nats.
-- You need some extra info at each step - what is it?
