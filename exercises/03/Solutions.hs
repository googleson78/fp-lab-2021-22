module Solutions where

import Prelude hiding (length, (++), zip, zipWith, reverse, concat, sum, take, product, drop, subtract, filter, map, all, and, null)

-- WEEK 3 TODO:
-- homework has points and deadline!
-- let, where

-- double :: Integer -> Integer
-- double x = x * x
-- ($) :: (a -> b) -> a -> b
-- ($) f x = f x

quad :: Integer -> Integer
quad x = double $ double x
  where
    double :: Integer -> Integer
    double x = x * x

quad' :: Integer -> Integer
quad' x =
  let
    double :: Integer -> Integer
    double x = x * x
   in double (double x)

isZero' :: Integer -> Bool
isZero' 0 = True
isZero' _ = False

isZero :: Integer -> Bool
isZero n =
  case n + 10 of
    0 -> True
    _ -> False

-- quad with double
-- case
-- maybe, lists, cons name
-- list range
-- list comprehension
-- * mapping
-- * many lists
-- * filtering

-- data Maybe a
--   = Nothing
--   | Just a
--   deriving Show

-- Cons(truct)
-- cons
data List a
  = Nil -- []
  | Cons a (List a) -- (:)
  deriving Show

-- f :: Integer -> Integer -> Maybe Integer
-- f x y =
--   case safeDiv x y of
--     Nothing -> Nothing
--     Just z -> Just (z * 10)

null :: [a] -> Bool
null [] = True
null _ = False

-- xs
-- dog, dogs
-- x, xs
sum :: [Integer] -> Integer
sum [] = 0
sum (n : ns) = n + sum ns

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

cartesianProd :: [a] -> [b] -> [(a, b)]
cartesianProd xs ys = [(x, y) | x <- xs, y <- ys]

-- WEEK 4 TODO:
-- mention homework deadline, mention look at prs

-- mention that String == [Char]

--type Pesho = Bool
--
--not' :: Pesho -> Pesho
--not' True = False
--not' False = True

-- Maybe as "computation that can fail" Lists as "one of many"

x :: Maybe Int
x = Nothing

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x y =
  if y == 0
  then Nothing
  else Just $ x `div` y

-- (safeDiv x 5) + (safeDiv y 5)
-- x = 10
-- y = 15

addMaybes :: Maybe Integer -> Maybe Integer -> Maybe Integer
addMaybes = lift2Maybe (+)

multMaybes :: Maybe Integer -> Maybe Integer -> Maybe Integer
multMaybes = lift2Maybe (*)

lift2Maybe ::
  (a      ->       b ->       c) ->
  Maybe a -> Maybe b -> Maybe c
lift2Maybe f mx my =
  case mx of
    Nothing -> Nothing
    Just x ->
      case my of
        Nothing -> Nothing
        Just y -> Just $ f x y

xs :: [Int]
xs = [1,2,3]

mySqrt :: Float -> [Float]
mySqrt x = [sqrt x, - sqrt x]

-- sqrt : R -> R x R

lift2List ::
  (a  ->  b  ->  c) ->
  [a] -> [b] -> [c]
lift2List f xs ys = [f x y | x <- xs, y <- ys]


-- TODO: lists as "many results"
-- lift2Maybe
-- safeDiv
-- mySqrt :: Float -> [Float]
-- lift2List

-- EXERCISE
-- Generate all the numbers in the ("mathematical range") [n, m] in a list (inclusive).
-- EXAMPLES
-- >>> listFromRange 3 12
-- [3,4,5,6,7,8,9,10,11,12]
-- >>> listFromRange 8 6
-- []
listFromRange :: Integer -> Integer -> [Integer]
listFromRange n m =
  if n > m
  then []
  else n : listFromRange (succ n) m

-- EXERCISE
-- Multiply all the elements of a list
-- EXAMPLES
-- >>> product [2,4,8]
-- 64
-- >>> product []
-- 1
product :: [Integer] -> Integer
product [] = 1
product (x:xs) = x * product xs

-- EXERCISE
-- Implement factorial with prod and listFromRange
fact :: Integer -> Integer
fact = product . listFromRange 1

-- EXERCISE
-- Return a list of the numbers that divide the given number.
-- EXAMPLES
-- >>> divisors 5
-- [1,5]
-- >>> divisors 64
-- [1,2,4,8,16,32,64]
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]
divisors :: Integer -> [Integer]
divisors n = [x | x <- listFromRange 1 n, n `rem` x == 0]

-- EXERCISE
-- Implement prime number checking using listFromRange and divisors
-- EXAMPLES
-- >>> isPrime 7
-- True
-- >>> isPrime 8
-- False
isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

-- EXERCISE
-- Get the last element in a list.
-- EXAMPLES
-- >>> lastMaybe []
-- Nothing
-- >>> lastMaybe [1,2,3]
-- Just 3
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_:xs) = lastMaybe xs

-- EXERCISE
-- Calculate the length of a list.
-- EXAMPLES
-- >>> length [1,2,8]
-- 3
-- >>> length []
-- 0
length :: [a] -> Integer
length [] = 0
length (_:xs) = succ $ length xs

-- EXERCISE
-- Return the nth element from a list (we count from 0).
-- If n >= length xs, return a Nothing
-- EXAMPLES
-- >>> ix 2 [1,42,69]
-- Just 69
-- >>> ix 3 [1,42,69]
-- Nothing
ix :: Integer -> [a] -> Maybe a
ix _ [] = Nothing
ix 0 (x:_) = Just x
ix n (_:xs) = ix (n - 1) xs

-- EXERCISE
-- "Drop" the first n elements of a list.
-- If n > length xs, then you should drop them all.
-- EXAMPLES
-- >>> drop 5 $ listFromRange 1 10
-- [6,7,8,9,10]
-- >>> drop 20 $ listFromRange 1 10
-- []
drop :: Integer -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (x:xs) = drop (n - 1) xs

-- EXERCISE
-- "Take" the first n elements of a list.
-- If n > length xs, then you should take as many as you can.
-- EXAMPLES
-- >>> take 5 $ listFromRange 1 10
-- [1,2,3,4,5]
-- >>> take 20 $ listFromRange 1 10
-- [1,2,3,4,5,6,7,8,9,10]
take :: Integer -> [a] -> [a]
take 0 xs = []
take n [] = []
take n (x : xs) = x : take (n - 1) xs

-- EXERCISE
-- Append one list to another. append [1,2,3] [4,5,6] == [1,2,3,4,5,6]
-- This is called (++) in the base library.
-- HINT: Of course, you can think in the classic "inductive" way - I've got the result - what do I need to do at this step.
-- Or alternatively, you can think about "placing the second list at the end of the first":
-- 0. how do you get to the end of the first one?
-- 1. what do you need to do at each step to "remember" the elements of the first one?
-- EXAMPLES
-- >>> append [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
-- >>> append [] [4,5,6]
-- [4,5,6]
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

-- EXERCISE
-- Concatenate all the lists together.
-- EXAMPLES
-- >>> concat [[1,2,3], [42,69], [5,7,8,9]]
-- [1,2,3,42,69,5,7,8,9]
-- >>> concat [[1,2,3], [], [5,7,8,9]]
-- [1,2,3,5,7,8,9]
-- >>> concat []
-- []
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = append xs $ concat xss

-- EXERCISE
-- Reverse a list. It's fine to do this however you like.
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs `append` [x]

-- EXERCISE
-- Square all the numbers in a list
-- EXAMPLES
-- >>> squareList [1,2,3,5]
-- [1,4,9,25]
squareList :: [Integer] -> [Integer]
squareList [] = []
squareList (x:xs) = x * x : squareList xs

-- EXERCISE
-- Pair up the given element with each of the elements a list.
-- EXAMPLES
-- >>> megaPair 42 [69,7,42]
-- [(42,69),(42,7),(42,42)]
megaPair :: a -> [b] -> [(a, b)]
megaPair x [] = []
megaPair x (y : ys) = (x, y) : megaPair x ys

-- EXERCISE
-- Both of those functions above have the same structure - apply a function to each element of a list.
-- We can abstract this and get one of the most useful functions over lists (and containers in general).
-- EXAMPLES
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> map (\x -> x * x) [1,2,3] -- same as squareList
-- [1,4,9]
-- >>> map (\x -> (3,x)) [1,2,3] -- same as megaPair 3
-- [(3,1),(3,2),(3,3)]
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- EXERCISE
-- Check if all the elements in a list are True.
-- EXAMPLES
-- >>> and []
-- True
-- >>> and [False]
-- False
-- >>> and [True, True]
-- True
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- EXERCISE
-- Check if all the elements of a list satisfy a predicate
-- Implement this using map and and.
-- EXAMPLES
-- >>> all isPrime [2,3,7]
-- True
-- >>> all isPrime [1,2,3,7]
-- False
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p
-- all p xs = and $ map p xs

-- EXERCISE
-- Implement the cartesian product of two lists.
-- Don't use a list comprehension!
-- >>> cartesian [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- >>> cartesian [] [4,5,6]
-- []
-- >>> cartesian [1,2,3] []
-- []
cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = concat $ map (\x -> map (\y -> (x, y)) ys) xs

-- EXERCISE
-- Again, we can generalise cartesian to work with arbitrary functions instead of just (,),
-- taking elements "each with each"
-- This is also the generalisation of cartesian, as seen in the examples.
-- NOTE: this was an exercise in week 3, but I've shown it live in week 4, so you can just ignore it.
-- EXAMPLES
-- >>> lift2List (+) [1] [2]
-- [3]
-- >>> lift2List (+) [1,2] [2]
-- [3,4]
-- >>> lift2List (*) [1,2,3] [1,2]
-- [1,2,2,4,3,6]
-- >>> lift2List (,) [1,2,3] [4,5,6] -- same as cartesian [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
lift2List' :: (a -> b -> c) -> [a] -> [b] -> [c]
lift2List' f xs ys = map (uncurry f) $ cartesian xs ys
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f x y = f (x, y)

--lift2List' f xs ys = concat $ map (\x -> map (\y -> f x y) ys) xs
--lift2List' f xs ys = concat $ map (\x -> map (f x) ys) xs

-- EXERCISE
-- The "filtering" part of a list comprehension - leave only those elements, that satisfy the given predicate.
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
filter p [] = []
filter p (x:xs) =
  if p x
  then x : filter p xs
  else filter p xs

--filter p (x:xs) = f $ filter p xs
--  where
--    f = if p x then (x:) else id

-- EXERCISE
-- Parse a character into a digit.
-- EXAMPLES
-- >>> parseDigit '6'
-- Just 6
-- >>> parseDigit '9'
-- Just 9
-- >>> parseDigit 'c'
-- Nothing
parseDigit :: Char -> Maybe Integer
parseDigit c =
  case c of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    _ -> Nothing

-- EXERCISE
-- See if all the values in a list xs are Just, returning Just xs only if they are.
-- We can think of this as all the computations in a list "succeeding",
-- and therefore the entire "computation list" has "succeeded.
-- Note that it is vacuously that all the elements in the empty list are Just.
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
validateList [] = Just []
validateList (x:xs) =
  case x of
    Nothing -> Nothing
    Just x' ->
      case validateList xs of
        Nothing -> Nothing
        Just xs' -> Just $ x' : xs'
--validateList (x:xs) =
--  case (x, validateList xs) of
--    (Just x', Just xs') -> Just $ x' : xs'
--    _ -> Nothing

-- EXERCISE
-- You often have a collection (list) of things, for each of which you want to
-- perform some computation, that might fail (returning Maybe).
-- Let's implement a function to do exactly this -
-- execute a "failing computation" for all the items in a list,
-- immediately "aborting" upon a failure.
-- Think about how to reuse validateList.
-- This is called traverseListMaybe, because it's a special case of a generic function called traverse
-- that performs "actions" for each element of a "collection", specialised to List and Maybe
-- EXAMPLES
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [2,4,6]
-- Just [2,4,6]
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [1,2,3]
-- Nothing
-- >>> traverseListMaybe (5 `safeDiv`) [0,2]
-- Nothing
-- >>> traverseListMaybe (8 `safeDiv`) [3,2]
-- Just [2,4]
traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe f xs = validateList $ map f xs
--traverseListMaybe f = validateList . map f

-- EXERCISE
-- Convert a list of digits to a number. Assume that the input list only has Integers
-- in the range [0,9]. Let's assume the empty list converts to 0.
-- HINT: It might be easier to first reverse the list and then operate on it with a helper.
-- EXAMPLES
-- >>> digitsToNumber [6,9]
-- 69
-- >>> digitsToNumber [1,2,0]
-- 120
-- >>> digitsToNumber [0,1,2,0]
-- 120
digitsToNumber :: [Integer] -> Integer
digitsToNumber = go . reverse
  where
    -- for some reason, we often call helpers in haskell "go", as in "go do the thing"
    go [] = 0
    go (x:xs) = x + 10 * go xs

-- EXERCISE
-- Combine the previous functions to parse a number.
-- EXAMPLES
-- >>> parseNumber "0"
-- Just 0
-- >>> parseNumber "3"
-- Just 3
-- >>> parseNumber "69"
-- Just 69
-- >>> parseNumber "0123"
-- Just 123
-- >>> parseNumber "blabla"
-- Nothing
-- >>> parseNumber "133t"
-- Nothing
parseNumber :: String -> Maybe Integer
parseNumber str =
  case traverseListMaybe parseDigit str of
    Nothing -> Nothing
    Just ns -> Just $ digitsToNumber ns
-- parseNumber =
--   maybeMap digitsToNumber . traverseListMaybe parseDigit

-- EXERCISE
-- Notice how in parseNumber, in the Nothing case we returned Nothing,
-- and in the Just case, we returned Just again, with a "non-maybe" function inside.
-- This turns out to be very useful, and if you compare it to the map for lists, it's almost the same.
-- Let's write it now, so we don't have to do that pattern match again in the future.
-- Afterwards, you can reuse this function in parseNumber.
-- EXAMPLES
-- >>> maybeMap succ $ Just 5
-- Just 6
-- >>> maybeMap succ Nothing
-- Nothing
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just $ f x

-- EXERCISE
-- Another way to combine lists
-- Instead of "taking all possible combinations" we group the lists "pointwise"
-- If one list is shorter than the other, you can stop there.
-- EXAMPLES
-- >>> zip [1,2,3] [4,5,6]
-- [(1,4),(2,5),(3,6)]
-- >>> zip [1,2] []
-- []
-- >>> zip [1] [4,5,6]
-- [(1,4)]
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

-- EXERCISE
-- And the generalised version of zip.
-- EXAMPLES
-- >>> zipWith (,) [1,2,3] [4,5]
-- [(1,4),(2,5)]
-- >>> zipWith (+) [1,2,3] [4,5,6]
-- [5,7,9]
-- >>> zipWith (:) [1,2,3] [[4],[5,7],[]]
-- [[1,4],[2,5,7],[3]]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs ys = map (uncurry f) $ zip xs ys

-- EXERCISE
-- Transpose a matrix. Assume all the inner lists have the same length.
-- HINT: zipWith and map might be useful here.
-- EXAMPLES
-- >>> transpose [[1]]
-- [[1]]
-- >>> transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
-- >>> transpose [[1],[2]]
-- [[1,2]]
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [xs] = map singleton xs
  where
    singleton x = [x]
    -- singleton = (:[])
transpose (xs:xss) = zipWith (:) xs $ transpose xss

-- alternative, using laziness:

-- in the base case, we would like to return "at least as many empty lists as there are columns in our original matrix",
-- so that in our first recursive step, all of the elements in our list have something to zip with
-- but we don't know how many there were in the beginning, when we reach the base case (unless with pass it down, but that's "ugly")
-- the "patch" in the solution above, is to add another base case, which guarantess the property with the columns
-- another solution is to just generate an infinite amount of empty lists, which also guarantees our property:
-- repeat x gives an infinite list of x, it's defined like this
-- repeat x = x : repeat x
--transpose [] = repeat []
--transpose (xs:xss) = zipWith (:) xs $ transpose xss



-- EXERCISE
-- Reverse a list, but in linear time (so if the input list has n elements, you should only be doing at most ~n operations, not n^2)
-- You will need a helper local definition.
-- EXAMPLES
-- >>> reverseLinear [1,2,3]
-- [3,2,1]
-- >>> reverseLinear []
-- []
reverseLinear :: [a] -> [a]
reverseLinear = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs
