{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

module Solutions where

-- use pattern matches!
-- >>> fact 5
-- 120
-- >>> fact 7
-- 5040
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- >>> fib 0
-- 0
-- >>> fib 4
-- 3
-- >>> fib 8
-- 21
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- use the following "mathematical definition" to implement addition on natural numbers:
-- myPlus x y = y                if x == 0
-- myPlus x y = succ(pred(x) + y) else
-- note that succ and pred are alrady available
-- >>> myPlus 3 7
-- 10
myPlus :: Integer -> Integer -> Integer
myPlus 0 m = m
myPlus n m = succ (myPlus (pred n) m)

-- same as above, implement multiplication on natural numbers recursively, using pred and myPlus
-- >>> myMult 7 6
-- 42
myMult :: Integer -> Integer -> Integer
myMult 0 _ = 0
myMult n m = myPlus m (myMult (pred n) m)

-- Author's note: I factored this out to use it in both fastPow and isPrime
-- >>> 7 `divides` 42
-- True
-- >>> 5 `divides` 42
-- False
divides :: Integer -> Integer -> Bool
divides n m = m `rem` n == 0

-- use the property that x^(2*n) == (x*x)^n
-- this is logarithmic, instead of linear, in the second argument
-- >>> fastPow 3 4
-- 81
-- >>> fastPow 2 6
-- 64
fastPow :: Integer -> Integer -> Integer
fastPow _ 0 = 1
fastPow x n =
  if 2 `divides` n
  then fastPow (x * x) (n `div` 2)
  else x * fastPow x (pred n)

-- rem x y = what's the remainder of x when divided by y
-- >>> isPrime 5
-- True
-- >>> isPrime 6
-- False
-- >>> isPrime 13
-- True
isPrime :: Integer -> Bool
isPrime n = not (anyDividesInRange 2 (pred n))
  where
    -- iterate over all the values in the given range, and check if any of them divide n
    anyDividesInRange :: Integer -> Integer -> Bool
    anyDividesInRange start end =
      if start > end
      then False
      else start `divides` n || anyDividesInRange (succ start) end
      -- alternatively
      -- start <= end && (start `divides` n || anyDividesInRange (succ start) end)
