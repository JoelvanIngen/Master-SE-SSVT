module Exercise8 where

import Data.List
import Test.QuickCheck

--Time spent 150 min 

{-
Euclid numbers are defined as the product of the first n prime numbers plus 1. 
They themselves are not always prime (for example, 30031 is a composite number). 
However, a Euclid number is never divisible by any of the first n prime numbers. 
This means that Euclid numbers are either new prime numbers or have prime factors 
larger than the first n primes.
This property demonstrates that no finite list of primes can contain all prime 
numbers—a foundational idea in Euclid’s proof of the existence of infinitely many primes.
-}

--A lot of time was spent writing a prime number generator, 
--and at the same time, a lot of time was spent understanding the Sieve of Eratosthenes.

counterexamples :: [([Integer], Integer)]
counterexamples = [(take n primes,product(take n primes ) + 1) 
                    | n<-[1..] 
                    , not (isPrime( product(take n primes ) + 1))]

--Check all odd numbers not exceeding √n to see if none of them can divide n. 
--If so, it means that n is a prime number.
isPrime :: Integer -> Bool
isPrime n
    | n == 2 = True
    | n `mod` 2 == 0 = False
    | otherwise = all (\m -> n `mod` m /= 0) [3,5.. floor(sqrt(fromIntegral n))]

--Sieve of Eratosthenes prime number generation methods
primes :: [Integer]
primes = sieve[2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
    putStrLn "The first seven results"
    mapM_ print (take 7 counterexamples)
 






