
-- Time Spent: 120 min

-- Ordered list of properties:
-- The derangement must be the same length as the original list.
-- The derangement must contain the same elements as the original list.
-- All elements of the derangement must be in a different postion than their original position.

module Exercise4 where
import Data.List (permutations, sort, elemIndex)
import Test.QuickCheck
import Test.QuickCheck.Test (test)


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
    print $ "Correct: " ++ show correctTest

    -- All derangements of length n
    let n = 4
    let testAllDeran = and [testDerangement [0..n] x | x <- deran n]
    print $ "Test all derangements using deran: " ++ show testAllDeran
