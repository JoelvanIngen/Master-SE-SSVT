-- Time spent: 120 min

-- Extra dependency installed: primes

module Euler10 (euler10, main) where

import Data.Numbers.Primes

-- (Very slow) alternative solution if not using the import
{-
-- Basic prime finder from lab 0
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Finds all primes lower than n (exclusive)
primes' :: Integer -> [Integer]
primes' n = filter prime [2..n]

sumPrimes :: Integer -> Integer
sumPrimes 0 = 0
sumPrimes n
    | n > 0 = sum (primes' n)
    | otherwise = error "Negative number"
-}

-- Makes a list of all prime numbers lower than the input number (exclusive)
-- All inputs lower than 3 will therefore give 0
primesUnder :: Integer -> [Integer]
primesUnder n = takeWhile (< n) primes

-- Sums all primes lower than the input number
sumPrimes :: Integer -> Integer
sumPrimes = sum . primesUnder

-- Tests for 2 million to solve the actual problem
euler10 :: Integer
euler10 = sumPrimes 2000000

main :: Integer
main = euler10

{- Test description:
To test this function, the following things should be tested for:
It should test that not every input would give the same answer (no hardcoding)
Then, the outputs of those tests should be cross-checked with the result
of another (correct) algorithm for this problem (though this does not prove that both work).
A more indepth way is to test (for a set of inputs) if every number generated before summing
is infact a prime number below the input limit, and if the number summed is actually the
sum of all members.
-}