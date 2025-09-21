
import Math.NumberTheory.Primes.Testing (isPrime)
import Data.List (sort)


formatOutput :: [(Integer, Integer, Integer)] -> String
formatOutput [(x, y, z)] = show x ++ show y ++ show z


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


main :: IO ()
main = do
    print $ formatOutput $ generateTriplets generatePrimeCanidates
