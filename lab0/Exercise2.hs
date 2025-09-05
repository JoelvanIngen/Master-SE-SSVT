-- Time Spent: 120 min

module Exercise2 (exerciseMain) where

import System.Random

bucketRanges :: [(Float, Float)]
bucketRanges = [(0.0, 0.25), (0.25, 0.5), (0.5, 0.75), (0.75, 1.0)]

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

-- Creates a function that can count the amount of numbers in a list that are in a specific range
countInRange :: Float -> Float -> [Float] -> Int
countInRange lower upper = length . filter (\x -> x >= lower && x < upper)

-- Recieves a list of numbers, and returns a bucket of amount of numbers per bucket
fillBuckets :: [Float] -> [Int]
fillBuckets vals = [ countInRange lower upper vals | (lower, upper) <- bucketRanges ]

-- Calculate chi-squared test statistics
chiSquared :: [Int] -> Int -> Float
chiSquared observed expected =
    let e = fromIntegral expected
        obs = map fromIntegral observed
    in sum [ ((o - e) ** 2) / e | o <- obs ]

-- Critical chi calculated with https://www.danielsoper.com/statcalc/calculator.aspx?id=12
-- for 3 degrees of freedom (4 buckets - 1) and p = 0.05
-- Determines whether there is or isn't significant bias
checkBias :: Float -> Bool
checkBias chi2 = chi2 > 7.81472790

-- Reports bias to the stdout
reportBias :: Bool -> IO ()
reportBias True = putStrLn "Bias has been found"
reportBias False = putStrLn "No bias has been found"

exerciseMain :: IO ()
exerciseMain = do
    vals <- probs 10000
    let counts = fillBuckets vals
    putStr "Counts: "
    print counts

    let chi2 = chiSquared counts 2500
    putStr "Chi-2: "
    print chi2

    let bias = checkBias chi2
    reportBias bias
