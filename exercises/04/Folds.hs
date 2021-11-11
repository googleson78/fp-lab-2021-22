{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

{-# LANGUAGE RankNTypes #-}

module Folds where

import Prelude hiding (or, foldr, length, (++), zip, zipWith, reverse, concat, sum, take, product, drop, subtract, filter, map, all, and, null)

-- mention hoogle

-- "boolean blindness" - prefer returning Maybe/"a proof", instead of a Bool and doing an if :
-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
-- https://runtimeverification.com/blog/code-smell-boolean-blindness/
-- https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/

-- fold
-- catamorphism

data Nat
  = Zero
  | Suc Nat
  deriving Show

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Suc $ integerToNat $ n - 1

-- show how we reached map
squareList :: [Integer] -> [Integer]
squareList [] = []
squareList (x:xs) = x * x : squareList xs

megaPair :: a -> [b] -> [(a, b)]
megaPair _ [] = []
megaPair x (y:ys) = (x, y) : megaPair x ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

addNat :: Nat -> Nat -> Nat
addNat Zero m = m
addNat (Suc n) m = Suc (addNat n m)

multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat (Suc n) m = addNat m (multNat n m)

foldNat :: (a -> a) -> a -> Nat -> a
foldNat _ v Zero = v
foldNat f v (Suc n) = f (foldNat f v n)

addNat' :: Nat -> Nat -> Nat
addNat' n m = foldNat Suc m n

-- n * m
-- n пъти да съберем m със себе си
-- 0 * m
multNat' :: Nat -> Nat -> Nat
multNat' n m = foldNat (addNat m) Zero n
-- > multNat' (Suc (Suc Zero)) m
-- > foldNat (addNat m) Zero (Suc (Suc Zero))
-- > addNat m (foldNat (addNat m) Zero (Suc Zero))
-- > addNat m (addNat m (foldNat (addNat m) Zero Zero))

-- > Suc (Suc Zero))
-- > f   (f   v)

-- Suc (Suc (Suc (Suc (Suc (Suc Zero)))))

sum :: [Integer] -> Integer
sum [] = 0 -- 0 + x == x == x + 0
sum (x:xs) = x + sum xs

product :: [Integer] -> Integer
product [] = 1 -- 1 * x == x == x * 1
product (x:xs) = x * product xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = f x $ foldr f v xs

-- foldr f v [1,2,3]
-- f 1 $ foldr f v [2,3]
-- f 1 $ f 2 $ foldr f v [3]
-- f 1 $ f 2 $ f 3 $ foldr f v []
-- (:) 1 ((:) 2 ((:) 3 []))
-- f   1 (f   2 (f   3 v))

-- EXERCISE
-- Implement natToInteger using foldNat.
-- EXAMPLES
-- >>> natToInteger $ Suc $ Suc $ Suc Zero
-- 3
natToInteger :: Nat -> Integer
natToInteger = undefined

-- EXERCISE
-- Implement exponentiation(n ^ m) using foldNat.
-- EXAMPLES
-- >>> natToInteger $ expNat (integerToNat 2) (integerToNat 10)
-- 1024
expNat :: Nat -> Nat -> Nat
expNat n m = undefined

-- EXERCISE
-- Implement and using foldr
-- EXAMPLES
-- >>> and [False]
-- False
-- >>> and [True, True]
-- True
and :: [Bool] -> Bool
and = undefined

-- EXERCISE
-- Implement or using foldr
-- EXAMPLES
-- >>> or [False]
-- False
-- >>> or [True, True]
-- True
or :: [Bool] -> Bool
or = undefined

-- EXERCISE
-- Implement length using foldr
-- EXAMPLES
-- >>> length [1,2,8]
-- 3
-- >>> length []
-- 0
length :: [a] -> Integer
length = undefined

-- EXERCISE
-- Implement concat using foldr
-- >>> concat [[1,2,3], [42,69], [5,7,8,9]]
-- [1,2,3,42,69,5,7,8,9]
-- >>> concat [[1,2,3], [], [5,7,8,9]]
-- [1,2,3,5,7,8,9]
-- >>> concat []
-- []
concat :: [[a]] -> [a]
concat = undefined

-- EXERCISE
-- Implement reverse using foldr (it's fine to do this in O(n^2)
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverse :: [a] -> [a]
reverse = undefined

-- EXERCISE
-- Implement map using foldr
-- EXAMPLES
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> map (\x -> x * x) [1,2,3] -- same as squareList
-- [1,4,9]
-- >>> map (\x -> (3,x)) [1,2,3] -- same as megaPair 3
-- [(3,1),(3,2),(3,3)]
map :: (a -> b) -> [a] -> [b]
map f = undefined

-- EXERCISE
-- Implement filter using foldr
-- EXAMPLES
-- >>> even 2
-- True
-- >>> even 3
-- False
-- >>> filter even [1..10]
-- [2,4,6,8,10]
-- >>> filter isPrime [1..20]
-- [2,3,5,7,11,13,17,19]
filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

-- EXERCISE
-- Implement null using foldr
-- EXAMPLES
-- >>> null []
-- True
-- >>> null [1]
-- False
null :: [a] -> Bool
null = undefined

-- EXERCISE
-- Implement headMaybe using foldr
-- EXAMPLES
-- >>> headMaybe []
-- Nothing
-- >>> headMaybe [1,2,3]
-- Just 1
headMaybe :: [a] -> Maybe a
headMaybe = undefined

-- EXERCISE
-- Implement a function that splits a list into two based on a predicate p
-- those that satisfy p and those that don't.
-- EXAMPLES
-- >>> partition (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partition even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = undefined

-- EXERCISE
-- Implement partition using foldr
-- EXAMPLES
-- >>> partitionfoldr (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partitionfoldr even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partitionfoldr :: (a -> Bool) -> [a] -> ([a], [a])
partitionfoldr = undefined

-- EXERCISE
-- Implement validateList using foldr.
-- EXAMPLES
-- >>> validateList []
-- Just []
-- >>> validateList [Just 42, Just 6, Just 9]
-- Just [42,6,9]
-- >>> validateList [Nothing, Just 6, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothing
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing
validateList :: [Maybe a] -> Maybe [a]
validateList = undefined

-- EXERCISE
-- Look at the recursor for nats - foldNat. In there we replaced Nats constructors, with things.
-- Think about how a recursor for tuples should look like, and implement it.
-- foldTuple :: ?
-- foldTuple = undefined

-- EXERCISE
-- Same as above, but this time for Maybe
-- foldMaybe :: ?
-- foldMaybe = undefined

-- EXERCISE
-- Same as above, but this time for Either
-- Reminder: Either is defined like so:
-- data Either a b = Left a | Right b
--
-- foldEither :: ?
-- foldEither = undefined

-- EXERCISE
-- If Nats can be converted to "n times applications" via foldNat,
-- is it perhaps true that "n times applications" can also be converted to Nats somehow?
--
-- You can ignore this bit below if you want to - just assume the forall means "the passed function must be polymorphic over a"
-- START "forall explanation"
-- Usually when we have a polymorphic function, like id :: a -> a
-- the *caller* chooses what a will be - when I write id 'a', I instantiate a with Char, so id becomes id :: Char -> Char
-- However, here we will need our function to work for any a, and so we must *require* something of the caller -
-- that they provide a function working *for any* a - meaning *we*(the callee) can decide what a to apply it for.
-- As a further example, consider
-- f :: (a -> a) -> Bool
-- f g = g True
-- this does *not* compile - let's assume it did.
-- if we have
-- h :: Int -> Int
-- h x = x + 1
-- then the caller would be able to write f h, (as they pick what a is) which is not valid,
-- since h requires its argument and return types to be Int, and True :: Bool
-- instead
-- f :: (forall a. a -> a) -> Bool
-- f g = g True
-- compiles, and now the caller cannot do f h, since the passed h needs to work *for any* a, while h :: Int -> Int
-- END "forall explanation"
--
-- EXAMPLES
-- >>> iterateToNat (\f x -> f (f (f x)))
-- Suc (Suc (Suc Zero))
iterateToNat :: (forall a. (a -> a) -> a -> a) -> Nat
iterateToNat f = undefined

natToIterate :: Nat -> (a -> a) -> a -> a
natToIterate n f v = foldNat f v n

-- EXERCISE
-- Figure out how to extend foldNat so you can easily write factorial over nats.
-- You need some extra info at each step - what is it?
-- foldNat' :: _
-- foldNat' = undefined

-- EXERICSE
-- And then implement factorial using it.
fact :: Nat -> Nat
fact = undefined

type Natural = forall a. (a -> a) -> a -> a
-- EXERCISE
-- Hey, if we can convert between Natural (the type argument to iterateToNat), now with a synonym) and Nat without losing information
-- wouldn't that mean that they are equivalent, and we can do the same things with both?
-- let's reimplement some of them, with Natural
-- These are called "church encoded" natural numbers - they're used to encode natural numbers, when the only thing you "have" is functions.
--
-- Here's some exposition:
-- As you saw in the iterateToNat example, these Naturals are essentially applying some function to some value a number of times.
-- The idea is that we represent the number n as applying a function f n times to a value v.
-- For example:
-- 0 is represented by \f v -> v
zero :: Natural
zero f v = v
-- 1 is represented by \f v -> f v
-- 2 is represented by \f v -> f (f v)
-- 3 is represented by \f v -> f (f (f v))
-- and so on
-- With this function, we need to somehow "add another f".
-- EXAMPLES
-- >>> iterateToNat zero
-- Zero
-- >>> iterateToNat $ suc $ suc zero
-- Suc (Suc Zero)
-- >>> natToInteger $ iterateToNat $ suc $ natToIterate $ integerToNat 5
-- 6
suc :: Natural -> Natural
suc n = undefined

-- EXERCISE
-- We can also add these. Here we need to think about how to add f n times to another Natural.
-- Note that usually
-- add n m = n suc m
-- Would be ok as a definition, however, due to some constraints in how ghc works, is not accepted (resolved in ghc 9.2 with ImpredicatveTypes)
-- EXAMPLES
-- >>> iterateToNat $ add (suc (suc zero)) zero
-- Suc (Suc Zero)
-- >>> iterateToNat $ add (suc (suc zero)) (suc (suc (suc zero)))
-- Suc (Suc (Suc (Suc (Suc Zero))))
-- >>> natToInteger $ iterateToNat $ add (suc (suc zero)) (suc (suc (suc (suc zero))))
-- 6
add :: Natural -> Natural -> Natural
add n m = undefined

-- EXERCISE
-- Now multiply them
-- >>> iterateToNat $ mult (suc (suc zero)) zero
-- Zero
-- >>> iterateToNat $ mult zero (suc (suc zero))
-- Zero
-- >>> iterateToNat $ mult (suc (suc zero)) (suc (suc zero))
-- Suc (Suc (Suc (Suc Zero)))
-- >>> natToInteger $ iterateToNat $ mult (suc (suc zero)) (suc (suc (suc zero)))
-- 6
mult :: Natural -> Natural -> Natural
mult n m = undefined

-- Is the same true for lists? Is there some function type that is "isomorphic" to lists - you can convert
-- back and forth between lists and the function, without losing data? Like how Natural is to Nat
-- (or if you prefer - can you express lists by only using lambdas?)
