{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

{-# LANGUAGE GADTSyntax #-}

module ADTs where

-- TODO: talk about
-- * pls write on discord
-- * scoring table
-- * where from last week
-- * pragmas on the top of files
-- * first homework?

-- show:
-- * remind about sections
-- * mention inline evaluation of expressions
-- * RPS - "enum"
-- * ask about gadt or normal
-- * mention Bool
-- * deriving Show

data RPS

beats :: RPS -> RPS -> Bool
beats = undefined

-- struct Point {
--   float x;
--   float y;
-- };
-- Point somePoint = Point{5, 3};

-- regular syntax
data Point

somePoint :: Point
somePoint = undefined

-- gadt syntax
data Point1 where

somePoint1 :: Point1
somePoint1 = undefined

isInFirstQuadrant :: Point -> Bool
isInFirstQuadrant = undefined

invert :: Point -> Point
invert = undefined

data Colour

data Breed

data Animal

data Nat

addNat :: Nat -> Nat -> Nat
addNat = undefined

natToInteger :: Nat -> Integer
natToInteger = undefined

-- >>> integerToNat 0
-- Zero
-- >>> integerToNat 3
-- Suc (Suc (Suc Zero))
integerToNat :: Integer -> Nat
integerToNat = undefined

-- TASK
-- define what the "next" throw you can do is in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
next :: RPS -> RPS
next = undefined

-- TASK
-- define what it means for two RPS values to be equal
-- in general for a type, this would mean that the constructors must be equal
-- and all their contents should all so be equal (pointwise)
-- for an "enum" in particular, this only leaves the constructor check
-- EXAMPLES
-- >>> eqRPS Rock Rock
-- True
-- >>> eqRPS Rock Paper
-- False
eqRPS :: RPS -> RPS -> Bool
eqRPS = undefined

-- TASK
-- define a shorter version of beats by uisng next and eqRPS
-- EXAMPLES
-- >>> beats' Rock Paper
-- True
-- >>> beats' Rock Scissors
-- False
-- >>> beats' Paper Scissors
-- True
beats' :: RPS -> RPS -> Bool
beats' = undefined

-- TASK
-- multiply two @Nat@s recursively, much like we did with Ints last time
-- EXAMPLES
-- >>> multNat Zero (Suc (Suc (Suc Zero)))
-- Zero
-- >>> multNat (integerToNat 2) (integerToNat 3)
-- Suc (Suc (Suc (Suc (Suc (Suc Zero)))))
multNat :: Nat -> Nat -> Nat
multNat n m = undefined

-- TASK
-- calculate the larger of two @Nat@s recursively
-- EXAMPLES
-- >>> maxNat (Suc Zero) Zero
-- Suc Zero
-- >>> maxNat (Suc (Suc Zero)) Zero
-- Suc (Suc Zero)
-- >>> maxNat (Suc (Suc Zero)) (Suc (Suc (Suc (Suc Zero))))
-- Suc (Suc (Suc (Suc Zero)))
maxNat :: Nat -> Nat -> Nat
maxNat n m = undefined

-- TASK
-- Ordering is a datatype that is made to mean "the result of a comparison" or "the ordering between two things"
-- it's defined like so:
-- @data Ordering = LT | EQ | GT@
-- with the constructors being L(ess)T(han), EQ(ual) G(reater)T(han)
-- implement a comparison for @Nat@s, returning an @Ordering@
-- EXAMPLES
-- >>> compareNat (Suc Zero) (Suc Zero)
-- EQ
-- >>> compareNat Zero (Suc Zero)
-- LT
-- >>> compareNat (Suc Zero) Zero
-- GT
compareNat :: Nat -> Nat -> Ordering
compareNat n m = undefined

-- TASK
-- create a Shape datatype that is either a Circle or a Rectangle,
-- containing the required @Point@s and/or @Float@s to define them in an ordinary 2d coordinate system
data Shape

-- TASK
-- Calculate the area of a Shape
-- note the existence of the constant @pi@ and the function @abs@ to calculate absolute values
area :: Shape -> Float
area s = undefined

-- the "syntax" for a very basic "calculator" datatype
-- or alternatively, a very simple programming language
--
-- we can build up Expr(essions) by
-- * injecting integers as a value directly - Val
-- * stating that we want to add the result of two calculations - Plus
-- * stating that we want to multiply the result of two calculations - Mult
data Expr
  = Val Integer
  | Plus Expr Expr
  | Mult Expr Expr
  deriving Show

-- we can abuse sections to write "prettier" expressions

-- using these pragmas we set the associativity of the constructor functions
-- so that
-- Val 3 `Plus` Val 4 `Plus` Val 5
-- means
-- Val 3 `Plus` (Val 4 `Plus` Val 5)
-- we also define Mult with higher precedence, so that we can write the usual
-- Val 3 `Plus` Val 4 `Mult` Val 5
-- to mean
-- Val 3 `Plus` (Val 4 `Mult` Val 5)
-- infixr(ight)
infixr 7 `Plus`
infixr 8 `Mult`

-- TASK
-- and now that we have the syntactic structure of the computation we want to make
-- we can implement its semantics by writing an evaluator for our calculator
-- or alternatively an interpreter for our programming language
-- EXAMPLES
-- >>> eval (Val 3)
-- 3
-- >>> eval (Plus (Val 3) (Val 4))
-- 7
-- >>> eval (Val 33 `Mult` Val 36)
-- 1188
-- >>> eval ((Val 3 `Plus` Val 3) `Mult` Val 7)
-- 42
-- >>> eval (Val 3 `Plus` Val 3 `Mult` Val 7)
-- 24
eval :: Expr -> Integer
eval expr = undefined

-- TASK
-- add an If expression to our Expr language
-- by using other calculations for our "condition value"
-- extend eval so that it also works with the new If construction
-- interpreting 0 as "false" and any other value as "true"

-- TASK
-- add "the noise that the animal makes" to the Animal type
-- how should you model a "noise"?
-- how are you going to add it to the Animal type?
--
-- is the way you've added it more optimal - or is there something you could factor out so that
-- speak :: Animal -> String
-- is easier to write, without matching on both Dog and Cat?
