-- Time Spent: 120 min

module Exercise6 (main) where

import Data.List ( tails )

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y * y <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Finds 101 consecutive primes whose sum is also a prime
-- Returns the window, and the sum
consecutive101Prime :: ([Integer], Integer)
consecutive101Prime = case [ (w, s) | w <- windowsOf 101 primes, let s = sum w, prime s ] of
    (x:_) -> x
    
    -- Shouldn't happen but prevents type error
    []    -> error "Error: Empty window"

-- Generate windows with size n out of a sequence xs
windowsOf :: Int -> [a] -> [[a]]
windowsOf n xs = takeWhile ((== n) . length) $ map (take n) $ tails xs

main :: IO ()
main = do
    let result = consecutive101Prime -- Get the tuple
    case result of
        (firstPrime:_, sumPrime) -> do
            putStrLn $ "First prime of sequence: " ++ show firstPrime
            putStrLn $ "Sum of window: " ++ show sumPrime
            putStrLn $ "Sum of window is prime: " ++ show (prime sumPrime)

        -- Also shouldn't happen but once again type errors
        ([], _) -> putStrLn "Error: Empty window"
