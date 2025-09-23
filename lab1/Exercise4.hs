
-- Time Spent: 120 min

{-
- used dependencies:
cabal install QuickCheck

- Ordered list of properties:
The derangement must be the same length as the original list.
The derangement must contain the same elements as the original list.
All elements of the derangement must be in a different postion than their original position.

- Can you automate the process?
Yes, you can automate the process of checking if the derangements are correct by using quickCheck.

- Test report:
According to the wikipedia page on derangements: "a derangement is a permutation
of the elements of a set in which no element appears in its original position.
In other words, a derangement is a permutation that has no fixed points".
source: https://en.wikipedia.org/wiki/Derangement

Therefore we can test if all no elements appear in their original position.
We can test if the derangement has the same length as the original list because it is a permutation.
And because it is a permutation, we can test if both lists contain the same elements.

These test were ran on lists with incorrect derangements, where either the length, elements or position of elements where incorrect.
These tests all returned false which means that the tests correctly identified the incorrect derangements.

The tests were also ran on a correct derangement which resulted in a true output meaning it correctly identifies correct derangements.
Lastly it was run on the deran function which generates all derangements of a list and according to the tests all these derangements where correct.

So we can conclude that the tests and the functions are correct.

A range from 2 to 10 was chose both because negative numbers like n=0 (n-1=-1),
break the 0..n-1 logic that the assignment wants. when n=1 it gives [0..n-1] = [0]
which cannot have a derangement and not larger than 10 because thatakes too long.
-}

module Exercise4 where
import Data.List (permutations, sort, elemIndex)
import Test.QuickCheck


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-- http://geekyplatypus.com/generating-permutations-and-derangements-using-haskell/
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = and [x `elem` ys && (index x xs /= index x ys) | x <- xs] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs


deran:: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) (permutations [0..n-1])


-- Test if both lists have the same length.
testLength :: [Int] -> [Int] -> Bool
testLength xs ys = isDerangement xs ys --> (length xs == length ys)


-- Test is both lists contain the same elements.
testSameElements :: [Int] -> [Int] -> Bool
testSameElements xs ys = isDerangement xs ys --> (sort xs == sort ys)


-- Test if all elements of the original list are in a different position in the derangement.
testDifferentPositions :: [Int] -> [Int] -> Bool
testDifferentPositions xs ys = and [elemIndex x xs /= elemIndex x ys | x <- xs]


-- Test if all the properties of a derangement hold.
testDerangement :: [Int] -> [Int] -> Bool
testDerangement xs ys = testLength xs ys
                        && testSameElements xs ys
                        && testDifferentPositions xs ys


genSingleInput :: Gen Int
genSingleInput = choose (2, 10)


main :: IO ()
main = do
    -- Empty lists
    let emptyTest = testDerangement [] []
    print $ "Empty lists: " ++ show emptyTest

    -- Different length
    let differentLengthTest = testDerangement [0,1,2,3] [2,1,0]
    print $ "Different length: " ++ show differentLengthTest

    -- Different elements
    let differentElementsTest = testDerangement [0,1,2,3] [1,4,2,0]
    print $ "Different elements: " ++ show differentElementsTest

    -- Same position
    let samePositionTest = testDerangement [0,1,2,3] [0,3,2,1]
    print $ "Same position: " ++ show samePositionTest

    -- Correct
    let correctTest = testDerangement [0,1,2,3] [3,0,1,2]
    print $ "Correct example: " ++ show correctTest

    -- All derangements of length n
    n <- generate genSingleInput

    print "Quickchecks: "
    quickCheck $ forAll (elements (deran n)) $ \xs -> testLength [0..n-1] xs
    quickCheck $ forAll (elements (deran n)) $ \xs -> testSameElements [0..n-1] xs
    quickCheck $ forAll (elements (deran n)) $ \xs -> testDifferentPositions [0..n-1] xs
    quickCheck $ forAll (elements (deran n)) $ \xs -> testDerangement [0..n-1] xs

