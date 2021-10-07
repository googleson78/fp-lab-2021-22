{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
module Intro where

-- TODO: > whoami
-- Georgi Lyubenov, he/him, contact info in readme
-- ben4a?

-- TODO: administrivia - georgi look at README!
-- ask about vax, live or remote if possible
-- ask about git knowledge

-- TODO: exposition
-- garbage collection
-- expressive static types
-- sum types
-- pattern matching!
-- functions! higher-order functions
-- lazy - a boon and a curse

-- no arbitrary IO
-- no mutations
-- value/"pipeline" oriented (give bash example maybe)

-- no null everywhere
-- concise, very modular

-- all in all - the language allows you to make it harder for yourself to write garbage
-- very enjoyable to work with - I'm dumb and so I let my compiler do most of my work for me
-- used for things that **really** shouldn't break :) - e.g. banks (standard chartered), military :(

-- disadvantages
-- not ultra popular:
-- * harder to get a job
-- * some libraries might be outdated/not extremely optimised
-- * there aren't obvious "best ways" to do things sometimes
-- learning curve is very steep at the beginning - especially when you ~~have been abused~~
-- are coming from an "imperative" and/or untyped background
-- used a lot for cryptoBS - third biggest crypto is in hs

-- myths
-- not a silver bullet
-- it's still a tool, and tools require proper usage (https://pbs.twimg.com/media/E7Kc0OhVUAAV0xz?format=jpg&name=small)
-- monads aren't hard (in haskell), they're only scary sounding

-- TODO: syntax and values
-- ghci
-- calling functions
-- base types
-- function definition
-- type declarations
-- pattern matching
-- if
-- operators

{-
-- use pattern matches!
-- >>> fact 5
-- 120
-- >>> fact 7
-- 5040
fact :: Integer -> Integer
fact n = _

-- >>> fib 0
-- 0
-- >>> fib 4
-- 3
-- >>> fib 8
-- 21
fib :: Integer -> Integer
fib = _

-- use the following "mathematical definition" to implement addition on natural numbers:
-- myPlus x y = y                if x == 0
-- myPlus x y = succ(pred(x) + y) else
-- note that succ and pred are alrady available
myPlus :: Integer -> Integer -> Integer
myPlus n m = _

-- same as above, implement multiplication on natural numbers recursively, using pred and myPlus
myMult :: Integer -> Integer -> Integer
myMult n m = _

-- use the property that x^(2*n) == (x*x)^n
-- this is logarithmic, instead of linear, in the second argument
-- >>> fastPow 3 4
-- 81
-- >>> fastPow 2 6
-- 64
fastPow :: Integer -> Integer -> Integer
fastPow = _

-- rem x y = what's the remainder of x when divided by y
-- >>> isPrime 5
-- True
-- >>> isPrime 6
-- False
-- >>> isPrime 13
-- True
isPrime :: Integer -> Bool
isPrime n = _
  where
    divides :: Integer -> Integer -> Bool
    divides x y = _

    -- iterate over all the values in the given range, and check if any of them divide n
    anyDividesInRange :: Integer -> Integer -> Bool
    anyDividesInRange start end = _
-}
