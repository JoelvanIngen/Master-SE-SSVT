-- Time spent: 60 min

{-
The calculation for this strength is rather simple, just a percentage of the surviving mutants vs.
the total mutants introduced to the function. The more mutants are killed, the higher the score.
If using a subset from exercise 3, the results are...
-}

module Exercise4 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Control.Monad
import Exercise2 (countSurvivors)

strength :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]] -> (Integer -> [Integer]) -> IO Float
strength n props muts fut = do
    x <- countSurvivors n props muts fut
    return (((fromIntegral n - fromIntegral x) / fromIntegral n) * 100)


main :: IO ()
main = do
    z <- strength 1000 multiplicationTableProps mutators multiplicationTable
    print z