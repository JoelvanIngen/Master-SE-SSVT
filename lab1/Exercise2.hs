-- Time spent: x min.

module Exercise2 (main) where

import Test.QuickCheck (Gen, choose, quickCheck)

-- Final solution with help from:
-- https://stackoverflow.com/questions/59742656/haskell-implement-powerset-function-of-a-set
powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

cardinality :: [Int] -> Bool
cardinality xs = length (powerset xs) == 2 ^ length xs

makeSet :: Int -> [Int]
makeSet n 
        | n >= 0 = [1..n]
        | otherwise = error "Negative number"

testCardinality :: Int -> Bool
testCardinality = cardinality . makeSet

-- Using a number range of 0-20 since negatives would end the program with an
-- error and higher values take exponentially longer to run
genSingleInput :: Gen Integer
genSingleInput = choose (0, 20)

{- Test report: 
 /Just mention results and the fact that it's not all encompassing but a higher range would take forever/
-}

{-
|A| = n => |P(A)| = 2^n
take A is empty, |A| = 0 = n, but P(A) is {{}}, so |P(A)| = 1 = 2^0
This holds, so this should hold for n+1.
...
-}

{- Question 1:
The cardinality property itself is easy to test, since we can directly compare the
lengths of the set and its powerset according to the proven formula.
What is harder is applying this test to many cases, but to verify an instance
of it with any n is easy.
-}

{- Question 2:
As said in question 1, this test only verifies that the size of the powerset is 2^n
of a set of size n. It does not prove the hypothesis that this is always correct.
The proof is done by induction earlier on, not by the test.
-}

main :: IO ()
main = do
    -- TODO: make this only use non-negative numbers
    quickCheck testCardinality