{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

{-# LANGUAGE InstanceSigs #-}

module Typeclasses where

import Prelude hiding (Semigroup(..), Monoid(..), mconcat, foldMap, fold, mtimes)

-- ask about next week
-- remind about homework and projects

-- motivation: lookup, sort, insert
id' :: a -> a
id' x = x

-- f :: Integer -> Integer
-- g :: Integer -> Integer
-- f == g ????????
sort'' :: [a] -> [a]
sort'' = undefined

class MyEq a where
  eq :: a -> a -> Bool
  eq x y = not $ neq x y
  neq :: a -> a -> Bool
  neq x y = not $ eq x y


-- laws
-- x y, x == y -> y == x
-- ........................................ == y
-- y == ........................................
-- neq Dog Cat -> True

data Animal = Dog | Cat
  deriving (Show, Eq, Ord, Read)
  -- read :: String -> a

data Tuple a b = MkTuple a b
  deriving (Show, Eq, Ord)

instance MyEq Animal where
  neq :: Animal -> Animal -> Bool
  neq Cat Dog = True
  neq Dog Cat = True
  neq _ _ = False

-- [x, y] == [u, v]
-- x == u && y == v

instance MyEq a => MyEq [a] where
  eq [] [] = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys
  eq _ _ = False

-- Show
-- class Show a where
--   show :: a -> String
--
-- class Num a where
--   (+)
--   (*) .....
--   fromInteger :: Integer -> a

-- x <= y == x >= y

-- data Nat = Zero | Suc Nat

--instance Num Nat where
--  fromInteger 0 = Zero
--  fromInteger n = Suc $ fromInteger $ n - 1

--f :: Integer -> Integer
--f x = x + 1

-- lookup' Dog [(Dog, 5), (Cat, 6)]
-- Just 5
-- lookup' Dog [(Cat, 6)]
-- Nothing
--lookup' :: MyEq a => a -> [(a, b)] -> Maybe b
--lookup' _ [] = Nothing
--lookup' x ((k, v):ys) =
--  if eq x k
--  then undefined
--  else undefined

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- x == y = x <= y || y <= x
-- x == y = compare x y == EQ

-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool
--
--   compare :: a -> a -> Ordering

-- x <= y
-- True
-- x < y или x == y
-- data Ordering = LT | EQ | GT

-- interfaces/abstract classes vs type classes
-- Eq, Ord
-- class definition syntax
-- show with an enum?
-- instance definition syntax -- mention show instancesigs, superclass constraints
-- using type classes - constraints and solving
-- other examples: Show, Read, Enum, Integral, Num
-- mention how Show and Num are used for literals
-- default methods - Ord, Ordering
-- deriving is cool
--   show RPS beats using derived Enum and Eq
--   explain how derived Eq and Ord work
-- laws
-- Monoid (Semigroup?) -- write it out so students can see it
-- remind newtypes
-- only one instance per type - use newtypes

-- Monoid е група без обратни елементи
--
-- Integer, (+)
-- mempty = 0
-- (<>) = (+)
-- Integer, (*)
--
class Monoid a where
  mempty :: a
  (<>) :: a -> a -> a
-- полугрупа, Semigroup

-- (x <> y) <> z == x <> (y <> z)
-- class Semigroup a where
--   (<>) :: a -> a -> a

-- mempty <> x == x
-- x <> mempty == x
-- class Semigroup a => Monoid a where
--   mempty :: a

-- EXERCISE
-- implement (<=) using compare
-- EXAMPLES
-- >>> leq 3 5
-- True
-- >>> leq 5 5
-- True
-- >>> leq 6 5
-- False
leq :: Ord a => a -> a -> Bool
leq = undefined

-- EXERCISE
-- Implement compare using <=
-- EXAMPLES
-- >>> compare' 3 5
-- LT
-- >>> compare' 5 5
-- EQ
-- >>> compare' True False
-- GT
-- >>> compare' 'a' 'b'
-- LT
compare' :: Ord a => a -> a -> Ordering
compare' = undefined


-- Given a function to convert a values, compare them using the ordering in b
-- This function is useful partially applied, when we have.
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- >>> comparing fst (5, 0) (4, 69)
-- >>> comparing snd (5, 0) (4, 69)
-- GT
-- LT
comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing = undefined

data Nat = Zero | Suc Nat
  deriving Show

-- EXERCISE
-- Implement a Monoid insance for Nat based on addition
instance Monoid Nat where

-- EXERCISE
-- Implement a Monoid instnace for [a]
-- Note how regardless of what a is, [a] is always a Monoid.
-- This is called a "free" structure in mathematics.
instance Monoid [a] where

-- EXERCISE
-- Implement a Monoid instance for the Add newtype, assuming the underlying value is a Num.
-- You will need to put a *superclass constraint* on your instance.
-- instance Foo a => Bar a where
--          ^^^^^^^^ this bit here
-- Remember that Num has (+) and fromInteger as class functions.
-- We should be able to do something like this:
-- Add 3 <> Add 5 == Add 8
-- Add 3.0 <> Add 5.0 == Add 8.0
newtype Add a = Add a
  deriving Show
-- instance ...

-- EXERCISE
-- "Monoid multiplication"
-- mtimes 5 x is intuitively supposed to be the same as 5 * x,
-- in other words, x <> x <> x <> x <> x
-- EXAMPLES
-- >>> mtimes (Suc $ Suc Zero) $ Suc $ Suc $ Suc Zero
-- Suc (Suc (Suc (Suc (Suc (Suc Zero)))))
-- >>> mtimes (Suc $ Suc Zero) $ [1,2,3]
-- [1,2,3,1,2,3]
-- >>> mtimes (Suc $ Suc Zero) $ Add 21
-- Add 42
mtimes :: Monoid a => Nat -> a -> a
mtimes = undefined

-- EXERCISE
-- Combine a list of elements, assuming that the type in the list is a Monoid
-- EXAMPLES
-- >>> fold [Zero, Suc Zero, Suc (Suc Zero)]
-- Suc (Suc (Suc Zero))
-- >>> fold $ map Add [1..10]
-- Add 55
-- >>> fold $ [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
fold :: Monoid a => [a] -> a
fold = undefined

-- EXERCISE
-- "Fold" a Maybe using a monoid and a mapping function.
-- This is useful when you want to default a Nothing to some monoid.
-- >>> foldMapMaybe Add $ Just 3
-- Add 3
-- >>> foldMapMaybe Add Nothing
-- Add 0
-- >>> foldMapMaybe (:[]) $ Just 'a'
-- "a"
-- >>> foldMapMaybe (:[]) Nothing
-- []
foldMapMaybe :: Monoid b => (a -> b) -> Maybe a -> b
foldMapMaybe = undefined

-- EXERCISE
-- Fold a list using a mapping function. Try implementing this with foldr.
-- **Extremely** useful function.
-- EXAMPLES
-- >>> foldMap Add [1..10]
-- Add 55
foldMap :: Monoid b => (a -> b) -> [a] -> b
foldMap = undefined

-- EXERCISE
-- Given a list of key-value pairs, update the value for a given key, or if it doesn't exist
-- insert it with a default value. This is what the Maybe b is for - so that the caller
-- can supply a modifying function and a default value at the same time.
-- Think about what the constraint is you will require.
-- Is there a reason to use any other constraint?
-- EXAMPLES
-- we can put function definitions on one line if we separate the clauses with a ;
-- >>> let f Nothing = 5; f (Just x) = x * 5
-- >>> upsert f "pesho" []
-- [("pesho",5)]
-- >>> upsert f "pesho" [("gosho", 42)]
-- [("gosho",42),("pesho",5)]
-- >>> upsert f "pesho" [("gosho", 42), ("pesho", 84)]
-- [("gosho",42),("pesho",420)]
upsert :: Eq a => (Maybe b -> b) -> a -> [(a, b)] -> [(a, b)]
upsert = undefined

-- EXERCISE
-- For a given list, return a key-value list with the keys being the original elements,
-- and the values being how many times each element was present in the original list. (aka a histogram)
-- Think about what the minimal constraint is you will require.
-- EXAMPLES
-- >>> histo [1,2,3]
-- [(3,1),(2,1),(1,1)]
-- >>> histo "How much would could a wood chuck chuck if a wood chuck could chuck wood?"
-- [('?',1),('d',6),('o',10),('w',5),(' ',14),('k',4),('c',11),('u',8),('h',5),('l',3),('a',2),('f',1),('i',1),('m',1),('H',1)]
histo :: Eq a => [a] -> [(a, Integer)]
histo = undefined

-- EXERCISE
-- Insert a value into an ordered list. Write the constraint yourself
-- >>> insert 5 [1..10]
-- [1,2,3,4,5,5,6,7,8,9,10]
-- >>> insert 5 [2, 4 .. 42]
-- [2,4,5,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42]
insert :: Ord a => a -> [a] -> [a]
insert = undefined

-- EXERCISE
-- Implement insertion sort.
-- >>> sort [4,12,3,1,1,2,34]
-- [1,1,2,3,4,12,34]
sort :: Ord a => [a] -> [a]
sort = undefined

-- EXERCISE
-- Functions with the same domain and codomain form a monoid.
-- Implement it.
newtype Endo a = Endo (a -> a)

getEndo :: Endo a -> a -> a
getEndo (Endo f) = f

-- EXAMPLES
-- >>> getEndo (foldMap Endo [succ, succ, (*2), succ]) 5
-- 14
-- >>> getEndo (foldMap Endo [(3:), (++[1,2,3])]) [4,2]
-- [3,4,2,1,2,3]
instance Monoid (Endo a) where

-- EXERCISE
-- Implement foldr via foldMap, by using the Endo Monoid.
-- EXAMPLES
-- >>> foldr' (++) [] [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
-- >>> foldr' (+) 0 [1..10]
-- 55
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' = undefined
