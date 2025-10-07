-- Time spent: 60 min

{-
The calculation for this strength is rather simple, just a percentage of the surviving mutants vs.
the total mutants introduced to the function. The more mutants are killed, the higher the score.
The scores may change for ignoring redundant or equivalent mutants, but we do not have a way
to account for those.

The best properties seem to be tenElements and sumIsTriangleNumberTimesInput, likely
due to these being very broad and mutants of these easily killed. There is
a very specific thing the mutation has to adhere to for these to survive.
-}

module Exercise4 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Control.Monad
import Exercise2 (countSurvivors)
import Exercise3 (findMinimalPropertySubset)
import MultiplicationTable (prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput)

strength :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]] -> (Integer -> [Integer]) -> IO Float
strength n props muts fut = do
    x <- countSurvivors n props muts fut
    return (((fromIntegral n - fromIntegral x) / fromIntegral n) * 100)


main :: IO ()
main = do
    percent <- strength 1000 multiplicationTableProps mutators multiplicationTable
    print percent

    percent <- strength 4000 [prop_tenElements] mutators multiplicationTable
    print percent

    percent <- strength 4000 [prop_firstElementIsInput] mutators multiplicationTable
    print percent

    percent <- strength 4000 [prop_sumIsTriangleNumberTimesInput] mutators multiplicationTable
    print percent

    percent <- strength 4000 [prop_linear] mutators multiplicationTable
    print percent

    percent <- strength 4000 [prop_moduloIsZero] mutators multiplicationTable
    print percent