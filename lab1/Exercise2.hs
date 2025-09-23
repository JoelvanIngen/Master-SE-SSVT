-- Time spent: 210 min.

{- Test report: 
 /Just mention results and conclusions and the fact that it's not all encompassing but a higher range would take forever/

Some more tests could be made by testing if the results of the makeSet and powerset functions
actually return proper sets, to prove the property that testCardinality compares a set and its
superset.
-}

{-
-Assume statement |P(A)| = 2^n is true for natural number k (non-negative integer)
-We must prove that this is also true for k+1 (induction)
-Define two sets
--Set A: |A| = k        -> |P(A)| = 2^k
--Set B: |B| = k + 1	-> |P(B)| = 2^(k+1)
--Define set B as a union of set A and a single element x that is not in A
-Divide sets into two categories
--Subsets of B that do not contain the element x
--Subsets of B that do contain the element x
-Any subset that does not contain element x is also a subset of A, since the other elements are identical
-Any subset that does contain element x can be constructed from a subset of A with element x added
-There are 2^k subsets of A, so also 2^k subsets of B without the element x
-Subsets of B that do contain x can be created by adding element x to any subset of A, so that is also 2^k subsets
-Total number of subsets in B: 2^k (without element x) + 2^k (with element x)
-|P(B)| = 2^k + 2^k = 2 * 2^k = 2^(k+1)
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

module Exercise2 (main) where

import Test.QuickCheck (Gen, choose, quickCheck, forAll)

-- Final solution with help from:
-- https://stackoverflow.com/questions/59742656/haskell-implement-powerset-function-of-a-set
-- Outputs a different order than in the assignment, but sets are unordered so this does not matter
-- This is due to implementation being done with lists, but the ordered behaviour is irrelevant here
powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

cardinality :: [Int] -> Bool
cardinality xs = length (powerset xs) == 2 ^ length xs

-- A negative n cannot make a list of length n, but an empty set has a powerset so 0 works
makeSet :: Int -> [Int]
makeSet n 
        | n >= 0 = [1..n]
        | otherwise = error "Negative number"

testCardinality :: Int -> Bool
testCardinality = cardinality . makeSet

-- Using a number range of 0-20 since negatives would end the program with an
-- error and higher values take exponentially longer to run
genSingleInput :: Gen Int
genSingleInput = choose (0, 20)

main :: IO ()
main = do
    quickCheck $ forAll genSingleInput testCardinality
