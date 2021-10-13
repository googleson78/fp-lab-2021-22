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

-- struct Point {
--   int x;
--   int y;
-- };
-- Point somePoint = Point{5, 3};

-- show:
-- "enum"
-- * RPS
-- * ask about gadt or normal
-- * mention Bool
-- * deriving Show
-- product types - tuple?
-- sum types - animal, animaltype

data Colour
data Breed

data Animal
-- recursive types - Nat.
-- "calculator"

data RPS

beats :: RPS -> RPS -> Bool
beats = undefined

-- task
next :: RPS -> RPS
next = undefined

-- task
eqRPS :: RPS -> RPS -> Bool
eqRPS = undefined

-- task
-- >>> beats' Rock Paper
-- True
-- >>> beats' Rock Scissors
-- False
-- >>> beats' Paper Scissors
-- True
beats' :: RPS -> RPS -> Bool
beats' = undefined

data Point = MkPoint Int Int

somePoint :: Point
somePoint = MkPoint 5 3

data Point1 where
  MkPoint1 :: Int -> Int -> Point1

somePoint1 :: Point1
somePoint1 = MkPoint1 5 3

isInFirstQuadrant :: Point -> Bool
isInFirstQuadrant = undefined

invert :: Point -> Point
invert = undefined

data Nat

addNat :: Nat -> Nat -> Nat
addNat = undefined

-- task
multNat :: Nat -> Nat -> Nat
multNat = undefined

-- task
maxNat :: Nat -> Nat -> Nat
maxNat = undefined

-- task
-- circle or square
data Shape

area :: Shape -> Int
area = undefined

data Calc
  = Val Int
  | Plus Int Int
  | Mult Int Int

-- task
eval :: Calc -> Int
eval = undefined

-- task add if, consider 0 false and /=0 true, extend eval

-- task, maybe
-- add "the noise that the animal makes" to the Animal type
-- is there a way to refactor it so that
-- speak :: Animal -> String
-- is easier to write, than matching on Dog and Cat?
