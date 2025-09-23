
-- Time spend: 120 min

module Euler49 where
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List (sort)


formatOutput :: [(Integer, Integer, Integer)] -> Integer
formatOutput [] = 0
formatOutput [(x, y, z)] = read $ show x ++ show y ++ show z


isPermuatation :: (Integer, Integer, Integer) -> Bool
isPermuatation (x, y, z) = (sort . show) x == (sort . show) y
                            && (sort . show) y == (sort . show) z


generateTriplets :: [Integer] -> [(Integer, Integer, Integer)]
generateTriplets xs = [(a, b, c) |
                       a <- xs,
                       b <- xs,
                       a < b,
                       b == a + 3330,
                       c <- xs,
                       c == b + 3330,
                       isPermuatation (a, b, c),
                       (a, b, c) /= (1487, 4817, 8147)]


generatePrimeCanidates :: [Integer]
generatePrimeCanidates = filter isPrime [1000..9999]


euler49 :: Integer
euler49 = formatOutput $ generateTriplets generatePrimeCanidates


main :: IO ()
main = do
    print euler49


{-
I would test the functions in the following way:
Check if all answers are 12-digits long.
According to the question there is only 1 answer so check if there is only 1 output.
Seperate the 12-digit answer into 3, 4-digit triplets.
Check if all numbers in the triples are:
- Prime numbers
- Spaced are spaced out by 3330
- Permutations of eachother
-}
