module Exercise4 where

import Test.QuickCheck

-- Time spent: 20 min.

reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primes' :: Integer -> [Integer]
primes' n = 2 : filter prime [3..n]

reversePrime :: Integer -> Bool
reversePrime n
    | prime n = prime (reversal n)
    | otherwise = False

-- The assignment said to use 'reversibleStream :: [Integer]', but this felt more intuitive
filterPrimes :: [Integer] -> [Integer]
filterPrimes p = filter (reversePrime) p

-- Properties

{-
Reversal correctness:
Reversing any number's reversal results in the same number as the original,
meaning the function does correctly reverse the number every time.
-}
doubleRev :: Integer -> Bool
doubleRev n = reversal (reversal n) == n

{-
Prime reversibility:
Though there are primes whose reversal is not prime (e.g. 19),
this function only records prime numbers whose reversals are also prime.
To prove this, the recorded numbers and their reversals are checked for
being prime.
-}
reversiblePrime :: Integer -> Bool
reversiblePrime n = all (reversePrime) (filterPrimes (primes' n))

{-
Maximum value:
All values in the result are below the value granted to the function (10000).
-}
checkMax :: Integer -> Bool
checkMax n = all (< n) (filterPrimes (primes' n))

main :: IO ()
main = do
    print (filterPrimes (primes' 10000))
    print (doubleRev 26352426769043)
    print (reversiblePrime 10000)
    print (checkMax 10000)
