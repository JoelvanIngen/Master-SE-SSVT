-- Time spent: 60 min

{-
The calculation for this strength is rather simple, just a percentage of the surviving mutants vs.
the total mutants introduced to the function. The more mutants are killed, the higher the score.
If using a subset from exercise 3, the results are...
-}

module Exercise4 where

import Data.List
import Mutation
import MultiplicationTable
import Control.Monad
import Exercise2 (countSurvivors)

strength :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Float
strength n props fut = do
    x <- countSurvivors n props fut
    return (((fromIntegral n - fromIntegral x) / fromIntegral n) * 100)


main :: IO ()
main = do
    z <- strength 1000 multiplicationTableProps multiplicationTable
    print z