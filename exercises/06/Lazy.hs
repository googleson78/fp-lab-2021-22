{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

module Lazy where

import Prelude hiding (scanl, until, foldl)
import Debug.Trace (traceShow)


-- x = x
--
-- x = 1 + x

--
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (_:xs) = take' (n - 1) xs

--x :: Int -> Int
--x = x

--f :: .. -> .. -> u
--f x y = (z :: u)

data Nat = Zero | Suc Nat
  deriving Show

--x :: Nat
--x = Suc x

isZero :: Nat -> Bool
isZero Zero = True
isZero (Suc _) = False

-- x = Suc x

-- data Stream a
-- ones = undefined

ones :: [Integer]
ones = 1 : ones

-- data Stream a = Cons a (Stream a)
--   deriving Show

-- onesStream :: Stream Integer
-- onesStream = Cons 1 onesStream
-- []

-- x :: Integer
-- x = 5 + 10
--
-- y :: [Integer]
-- y = [2 * 2, 4 * 4, 5 * 5, 6]
--
-- z :: (Integer, [Integer])
-- z = (5 * 5, [1,2,3])

-- foldr - не е опашко рекурсивно :(
-- foldl - опашко рекурсивно :)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v [] = v
foldl f v (x:xs) = foldl f (f v x) xs

-- thunk
-- "сметка която не е оценена"
-- foldl (+) 0 [1,2,3] --
-- foldl f 1 [2,3]
-- foldl f 3 [3]
-- foldl f 6 []

-- seq
-- > :t seq
-- seq :: a -> b -> b
-- ако (seq x y) е оценено, тогава със сигурност x и y ще са оценени
-- x y (seq x y)
-- y x (seq x y)
-- невъзможно - y (seq x y) x

-- foldl - не
-- foldl' - да
-- import Data.List (foldl')

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = seq (f v x) (foldl' f (f v x) xs)

-- deepseq

-- explain and show :sprint
-- when does evaluation happen?
-- why does foldl leak
-- seq
-- how to make it not leak? seq
-- A note on evaluation order: the expression seq a b does not guarantee that a will be evaluated before b.
-- The only guarantee given by seq is that the both a and b will be evaluated before seq returns a value.
-- In particular, this means that b may be evaluated before a.
-- and with tuples?

-- EXERCISE
-- A list of all the natural numbers.
-- EXAMPLES
-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]
nats :: [Integer]
nats = undefined

-- EXERCISE
-- Generate an infinite list of numbers, starting with the given number, with the given interval between each numbe.
-- EXAMPLES
-- >>> take 10 $ fromThen 0 1
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> take 20 $ fromThen 0 1
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
-- >>> take 10 $ fromThen 4 9
-- [4,13,22,31,40,49,58,67,76,85]
-- >>> take 10 $ fromThen 0 (-10)
-- [0,-10,-20,-30,-40,-50,-60,-70,-80,-90]
fromThen :: Integer -> Integer -> [Integer]
fromThen = undefined

-- EXERCISE
-- Implement a list of all the factorial numbers
-- Use a where or let to "cache" the current number, so we don't do all the multiplications every time.
-- i.e. if we've already calculated 5!, we can simply multiply the result by 6, we don't need to calculate 5! again.
-- EXAMPLES
-- >>> take 10 facts
-- [1,1,2,6,24,120,720,5040,40320,362880]
facts :: [Integer]
facts = undefined
  where
    go = undefined

-- EXERCISE
-- "Caching foldl"
-- It's sometimes useful to have all the "intermediate" results of a fold. It's also helpful for debugging sometimes.
-- Implement a version of foldl that returns all of the intermediate results it has.
-- These are called "scans" in the Haskell standard library.
-- EXAMPLES
-- >>> scanl (+) 0 [1..10]
-- [1,3,6,10,15,21,28,36,45,55,55]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl = undefined

-- EXERCISE
-- Use scanl to implement facts.
-- EXAMPLES
-- >>> take 10 factsScanl
-- [1,2,6,24,120,720,5040,40320,362880,3628800]
factsScanl :: [Integer]
factsScanl = undefined

-- EXERCISE
-- Implement a list of all the fibonacci numbers.
-- Use the following idea:
-- The fibonacci numbers start with 1 1
-- To generate the next fibonacci number, we need to sum the previous two, so assuming we already have
-- fibs :: [Integer]
-- that would mean summing the head of fibs with the head of the tail of fibs
-- zipWith will be useful here.
-- EXAMPLES
-- >>> take 10 fibs
-- [1,1,2,3,5,8,13,21,34,55]
fibs :: [Integer]
fibs = undefined

-- EXERCISE
-- Idea:
-- We can get the all the primes with the following algorithm.
-- Let's start with all the numbers >=2
-- The current number - call it x - is prime (this is true in the beginning - x = 2 is prime) - leave it in our list
-- All of the other numbers that are divisible by x aren't prime - filter them out
-- This is called the sieve of Eratosthenes
-- Implement this for a given input list
-- Now all we need to do is apply it to all the natural numbers >=2 to get a list of all the primes.
-- EXAMPLES
-- >>> take 10 $ primes
-- [2,3,5,7,11,13,17,19,23,29]
-- >>> take 20 $ primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
primes :: [Integer]
primes = undefined
  where
    eratosthenes :: [Integer] -> [Integer]
    eratosthenes = undefined

-- Let's consider the following problem:
-- We have a "circle" of n people. We have an integer k.
-- We repeat the following procedure, starting from the first person, until there is only one left:
-- Kill the kth person. Repeat this, starting from the k+1th person.
-- Example execution for n = 5, k = 2:
-- 1 2 3 4 5 (start)
-- 1 2 4 5 (we kill 3, since 1 + 2 = 2)
-- 2 4 5 (we kill 1, since 4 + 2 = 6, circling back to 1)
-- 2 4
-- 4

-- This is called the josephus problem.
-- Your task is to implement jos :: Integer -> Int -> Integer
-- which takes n and k, and returns the last surviving person.
-- Your hint is that you can "simulate" a cicle of people, by creating an infinitely looping list
-- (e.g. it would be [0,1,2,3,4,0,1,2,3,4,0,1,2,3,4...] for n = 5)
-- and then dropping elements from the list and filtering it, until there is "only one person" left in the list
-- EXAMPLES
-- >>> jos 5 2
-- 4
-- >>> jos 10 4
-- 3
-- >>> jos 10 8
-- 7
-- >>> map (\(x,y) -> (x, y, jos x y)) [(x, y) | x <- [2..5], y <- [2..5]]
-- [(2,2,2),(2,3,1),(2,4,2),(2,5,1),(3,2,2),(3,3,2),(3,4,1),(3,5,1),(4,2,1),(4,3,2),(4,4,2),(4,5,3),(5,2,4),(5,3,1),(5,4,2),(5,5,4)]
jos :: Integer -> Int -> Integer
jos = undefined
  where
    -- repeat a function until a condition turns True
    -- this function actually exists in the standard library
    -- EXAMPLES
    -- >>> until (>10) succ 0
    -- 11
    until :: (a -> Bool) -> (a -> a) -> a -> a
    until = undefined
    -- our infinite circle
    circle :: [Integer]
    circle = undefined
    -- the procedure which actually does the removal
    go :: [Integer] -> [Integer]
    go = undefined
